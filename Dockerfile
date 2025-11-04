# Use official Shiny Server base image
FROM rocker/shiny:4.3.2

# Install required system dependencies and curl (for healthcheck)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Copy app and data into Shiny Server directory
COPY app.R /srv/shiny-server/
COPY all_2016_2017_sur.csv /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

# Install R package dependencies
RUN R -e "install.packages(c('shiny', 'plotly', 'dplyr', 'readr', 'tidyr', 'tools'))"

# Expose Shiny Server port
EXPOSE 3838

# Add a health check every 60 minutes
HEALTHCHECK --interval=60m --timeout=10s --start-period=90s --retries=3 \
  CMD curl -fs http://localhost:3838 || exit 1

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]




