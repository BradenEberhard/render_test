# Start from the Rocker Shiny image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    imagemagick \
    libmagick++-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error \
    tidyverse \
    dotenv \
    RPostgres \
    DBI \
    plotly \
    future \
    promises \
    bslib \ 
    fontawesome \
    shinyWidgets \
    sysfonts \
    showtext \
    ggiraph \
    ggimage \
    shinycssloaders \
    pool \
    glue \
    DT

# Copy app
COPY . /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
