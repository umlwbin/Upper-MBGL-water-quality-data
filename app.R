library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(tidyr)


# ==============================================================================
# LIGHTWEIGHT HEALTH ENDPOINT
# ==============================================================================

# Define a simple /health handler that responds quickly without starting a session
health_handler <- function(req) {
  list(
    status = 200,
    headers = list('Content-Type' = 'text/plain'),
    body = "OK"
  )
}

# Register /health handler (before app starts)
options(shiny.http.response.handlers = list(
  health = function(req) {
    if (identical(req$PATH_INFO, "/health")) {
      return(health_handler(req))
    }
    NULL
  }
))



# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Upper Manitoba Great Lakes Water Quality, 2016-2017"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("year_slider",
                  "Year:",
                  min = 2016,
                  max = 2017,
                  value = 2016,
                  step = 1,
                  sep = "",
                  animate = animationOptions(interval = 2000, loop = TRUE)),

      selectInput("variable",
                  "Variable to Plot:",
                  choices = c("Conductivity" = "cond",
                              "pH" = "ph",
                              "Total Nitrogen (TN)" = "total_n",
                              "Suspended Nitrogen (SUSPN)" = "suspn",
                              "Total Dissolved Nitrogen (TDN)" = "tdn",
                              "Total Phosphorus (TP)" = "total_p",
                              "Suspended Phosphorus (SUSP)" = "susp",
                              "Total Dissolved Phosphorus (TDP)" = "tdp",
                              "Soluble Reactive Phosphorus (SRP)" = "srp",
                              "GF Chlorophyll-a (TotC)" = "gf_chl",
                              "Secchi Depth" = "secchi_depth",
                              "Total Suspended Solids (TSS)" = "TSS",
                              "Fixed Suspended Solids (FSS)" = "FSS",
                              "Particulate Organic Matter (POM)" = "POM",
                              "Volatile Suspended Solids (VSS)" = "VSS",
                              "Dissolved Organic Carbon (DOC)" = "doc",
                              "Suspended Carbon (SUSPC)" = "suspc",
                              "Susp C:N Ratio" = "Susp_C_N_uM_uM",
                              "Susp N:P Ratio" = "Susp_N_P_uM_uM",
                              "Susp C:P Ratio" = "Susp_C_P_uM_uM",
                              "Susp C:Chlorophyll-a" = "Susp_C_chla_uM_ug"),
                  selected = "cond"),

      selectInput("season_filter",
                  "Season Filter:",
                  choices = c("All Seasons" = "all",
                              "Spring" = "spring",
                              "Summer" = "summer",
                              "Fall" = "fall"),
                  selected = "all"),

      hr(),
      h4("Summary Statistics by Lake"),
      uiOutput("summary_stats")
    ),

    mainPanel(
      width = 9,
      plotlyOutput("map", height = "1000px"),  # Changed from 700px to 1000px for portrait

      # Footer
      hr(),
      div(style = "text-align: center; padding: 20px; background-color: #f8f9fa; border-top: 2px solid #dee2e6; margin-top: 20px;",
          tags$p(style = "margin: 0; color: #495057; font-size: 14px;",
                 tags$strong("Cite as: "),
                 "Claire Herbert (2025). Upper MBGL Water Quality Interactive Map, 2016-2017."
          )
      )
    )
  )
)


# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # Load and process data
  lake_data <- reactive({
    req(input$variable)

    # Read the CSV file
    df <- read_csv("all_2016_2017_sur.csv", show_col_types = FALSE)

    # Get the selected variable
    var_col <- input$variable

    # Check if column exists
    if (!var_col %in% names(df)) {
      return(NULL)
    }

    # Filter out rows with missing data for selected variable or coordinates
    df <- df %>%
      filter(!is.na(lat), !is.na(long), !is.na(station)) %>%
      filter(!is.na(.data[[var_col]]) & .data[[var_col]] != "NA")

    # Calculate median for selected variable by station, year, and season
    station_summary <- df %>%
      group_by(station, year, Season, lat, long, location, subloc) %>%
      summarise(
        median_value = median(.data[[var_col]], na.rm = TRUE),
        n_samples = n(),
        .groups = 'drop'
      ) %>%
      filter(!is.na(median_value) & is.finite(median_value))

    return(station_summary)
  })

  # Filtered data based on user selections
  map_data <- reactive({
    data <- lake_data()

    # Check if data exists
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }

    # Filter by year from slider
    plot_df <- data %>%
      filter(year == input$year_slider)

    # Apply season filter
    if (input$season_filter != "all") {
      plot_df <- plot_df %>%
        filter(tolower(Season) == input$season_filter)
    }

    # Check if we still have data
    if (nrow(plot_df) == 0) {
      return(data.frame())
    }

    # Aggregate by station (in case multiple seasons selected)
    plot_df <- plot_df %>%
      group_by(station, year, lat, long, location, subloc) %>%
      summarise(
        median_value = median(median_value, na.rm = TRUE),
        n_samples = sum(n_samples),
        seasons = paste(unique(Season), collapse = ", "),
        .groups = 'drop'
      ) %>%
      filter(!is.na(median_value) & is.finite(median_value))

    plot_df
  })

  # Get variable label and units
  var_info <- reactive({
    req(input$variable)

    var_labels <- list(
      "cond" = list(label = "Conductivity", unit = "µS/cm"),
      "ph" = list(label = "pH", unit = ""),
      "tdn" = list(label = "Total Dissolved Nitrogen", unit = "µg/L"),
      "suspn" = list(label = "Suspended Nitrogen", unit = "µg/L"),
      "srp" = list(label = "Soluble Reactive Phosphorus", unit = "µg/L"),
      "susp" = list(label = "Suspended Phosphorus", unit = "mg/L"),
      "tdp" = list(label = "Total Dissolved Phosphorus", unit = "µg/L"),
      "gf_chl" = list(label = "GF Chlorophyll-a (TotC)", unit = "µg/L"),
      "secchi_depth" = list(label = "Secchi Depth", unit = "m"),
      "TSS" = list(label = "Total Suspended Solids", unit = "mg/L"),
      "FSS" = list(label = "Fixed Suspended Solids", unit = "mg/L"),
      "VSS" = list(label = "Volatile Suspended Solids", unit = "mg/L"),
      "doc" = list(label = "Dissolved Organic Carbon", unit = "µmol/L"),
      "total_n" = list(label = "Total Nitrogen", unit = "µg/L"),
      "total_p" = list(label = "Total Phosphorus", unit = "µg/L"),
      "POM" = list(label = "Particulate Organic Matter", unit = "mg/L"),
      "suspc" = list(label = "Suspended Carbon", unit = "µg/L"),
      "Susp_C_N_uM_uM" = list(label = "Susp C:N Ratio", unit = ""),
      "Susp_N_P_uM_uM" = list(label = "Susp N:P Ratio", unit = ""),
      "Susp_C_P_uM_uM" = list(label = "Susp C:P Ratio", unit = ""),
      "Susp_C_chla_uM_ug" = list(label = "Susp C:Chlorophyll-a", unit = "")
    )

    if (input$variable %in% names(var_labels)) {
      return(var_labels[[input$variable]])
    } else {
      return(list(label = input$variable, unit = ""))
    }
  })

  # Create map
  output$map <- renderPlotly({
    req(input$variable, input$year_slider)

    df <- map_data()
    info <- var_info()

    if (is.null(df) || nrow(df) == 0) {
      return(plot_ly() %>%
               layout(
                 title = "No data available for selected filters",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }

    # Create hover text
    unit_text <- if(info$unit != "") paste0(" ", info$unit) else ""
    df <- df %>%
      mutate(
        hover_text = paste0(
          "<b>Station:</b> ", station, "<br>",
          "<b>Location:</b> ", location, " - ", subloc, "<br>",
          "<b>Year:</b> ", year, "<br>",
          "<b>Season(s):</b> ", seasons, "<br>",
          "<b>Median ", info$label, ":</b> ", round(median_value, 2), unit_text, "<br>",
          "<b>Samples:</b> ", n_samples
        )
      )

    p <- plot_ly(
      data = df,
      type = 'scattermapbox',
      mode = 'markers',
      lat = ~lat,
      lon = ~long,
      marker = list(
        size = 11,
        color = ~median_value,
        colorscale = list(c(0, '#440154'), c(0.25, '#31688e'), c(0.5, '#35b779'),
                          c(0.75, '#fde724'), c(1, '#ff0000')),
        showscale = TRUE,
        colorbar = list(
          title = paste0(info$label, if(info$unit != "") paste0("<br>(", info$unit, ")") else ""),
          x = 1.02
        ),
        opacity = 0.8,
        line = list(color = 'white', width = 1)
      ),
      hoverinfo = 'text',
      hovertext = ~hover_text,
      text = ~station,
      textposition = "top center"
    )

    # Configure map layout
    season_label <- if(input$season_filter == "all") "All Seasons" else tools::toTitleCase(input$season_filter)
    title_text <- paste0(info$label, " - ", input$year_slider, " (", season_label, ")")

    p %>%
      layout(
        mapbox = list(
          style = "open-street-map",
          center = list(lon = median(df$long, na.rm = TRUE), lat = median(df$lat, na.rm = TRUE)),
          zoom = 6.5
        ),
        title = list(text = title_text, x = 0.5),
        margin = list(l = 0, r = 0, t = 40, b = 0),
        showlegend = FALSE
      )
  })

  # Summary statistics by lake
  output$summary_stats <- renderUI({
    req(input$variable, input$year_slider)

    info <- var_info()
    var_col <- input$variable

    # Read the RAW CSV data directly
    df <- read_csv("all_2016_2017_sur.csv", show_col_types = FALSE)

    # Check if column exists
    if (!var_col %in% names(df)) {
      return(
        tags$div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
                 tags$p(style = "color: #6c757d; margin: 0;", "No data available")
        )
      )
    }

    # Filter raw data
    filtered_data <- df %>%
      filter(!is.na(lat), !is.na(long), !is.na(station)) %>%
      filter(!is.na(.data[[var_col]]) & .data[[var_col]] != "NA") %>%
      filter(year == input$year_slider)

    # Remove negative values for TSS, VSS, and FSS
    if (var_col %in% c("TSS", "VSS", "FSS")) {
      filtered_data <- filtered_data %>%
        filter(.data[[var_col]] >= 0)
    }

    # Apply season filter
    if (input$season_filter != "all") {
      filtered_data <- filtered_data %>%
        filter(tolower(Season) == input$season_filter)
    }

    if (nrow(filtered_data) == 0) {
      return(
        tags$div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
                 tags$p(style = "color: #6c757d; margin: 0;", "No data available for selected filters")
        )
      )
    }

    unit_text <- if(!is.null(info$unit) && info$unit != "") paste0(" ", info$unit) else ""
    season_label <- if(input$season_filter == "all") "All" else tools::toTitleCase(input$season_filter)

    # Group by location (lake) and calculate statistics from RAW VALUES
    lake_stats <- filtered_data %>%
      group_by(location) %>%
      summarise(
        n_stations = n_distinct(station),
        n_samples = n(),
        mean_val = round(mean(.data[[var_col]], na.rm = TRUE), 2),
        median_val = round(median(.data[[var_col]], na.rm = TRUE), 2),
        min_val = round(min(.data[[var_col]], na.rm = TRUE), 2),
        max_val = round(max(.data[[var_col]], na.rm = TRUE), 2),
        sd_val = round(sd(.data[[var_col]], na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      # Reorder lakes
      mutate(location = factor(location,
                               levels = c("LWPO", "LWH", "LMB"))) %>%
      arrange(location)

    # Create UI for each lake
    lake_divs <- lapply(1:nrow(lake_stats), function(i) {
      lake <- lake_stats[i, ]

      tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff; margin-bottom: 15px;",
               tags$div(style = "margin-bottom: 12px;",
                        tags$div(style = "font-weight: bold; color: #495057; margin-bottom: 8px; font-size: 16px;",
                                 tags$span(style = "color: #007bff;", as.character(lake$location))
                        ),
                        tags$div(style = "font-weight: bold; color: #495057; margin-bottom: 8px;",
                                 tags$span(style = "font-size: 13px;", "Year: "),
                                 tags$span(style = "color: #007bff; font-size: 13px;", input$year_slider)
                        ),
                        tags$div(style = "font-weight: bold; color: #495057; margin-bottom: 8px;",
                                 tags$span(style = "font-size: 13px;", "Season: "),
                                 tags$span(style = "color: #007bff; font-size: 13px;", season_label)
                        ),
                        tags$div(style = "font-weight: bold; color: #495057; margin-bottom: 8px;",
                                 tags$span(style = "font-size: 13px;", "Variable: "),
                                 tags$span(style = "color: #007bff; font-size: 13px;", info$label)
                        )
               ),
               tags$hr(style = "margin: 12px 0; border-top: 1px solid #dee2e6;"),
               tags$div(style = "margin-bottom: 10px;",
                        tags$div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
                                 tags$span(style = "color: #6c757d; font-size: 12px;", "Stations:"),
                                 tags$strong(style = "color: #212529; font-size: 12px;", lake$n_stations)
                        ),
                        tags$div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                                 tags$span(style = "color: #6c757d; font-size: 12px;", "Samples:"),
                                 tags$strong(style = "color: #212529; font-size: 12px;", lake$n_samples)
                        )
               ),
               tags$hr(style = "margin: 12px 0; border-top: 1px solid #dee2e6;"),
               tags$div(
                 tags$div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                          tags$span(style = "color: #6c757d; font-size: 12px;", "Mean:"),
                          tags$strong(style = "color: #212529; font-size: 12px;", paste0(lake$mean_val, unit_text))
                 ),
                 tags$div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                          tags$span(style = "color: #6c757d; font-size: 12px;", "Median:"),
                          tags$strong(style = "color: #212529; font-size: 12px;", paste0(lake$median_val, unit_text))
                 ),
                 tags$div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                          tags$span(style = "color: #6c757d; font-size: 12px;", "Min:"),
                          tags$strong(style = "color: #28a745; font-size: 12px;", paste0(lake$min_val, unit_text))
                 ),
                 tags$div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                          tags$span(style = "color: #6c757d; font-size: 12px;", "Max:"),
                          tags$strong(style = "color: #dc3545; font-size: 12px;", paste0(lake$max_val, unit_text))
                 ),
                 tags$div(style = "display: flex; justify-content: space-between;",
                          tags$span(style = "color: #6c757d; font-size: 12px;", "SD:"),
                          tags$strong(style = "color: #212529; font-size: 12px;", paste0(lake$sd_val, unit_text))
                 )
               )
      )
    })

    # Return all lake divs
    tagList(lake_divs)
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)
