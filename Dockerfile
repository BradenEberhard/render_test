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

COPY ./fonts/ /usr/share/fonts/truetype/

# Update font cache to include the new fonts
RUN fc-cache -f -v

# Copy app
COPY . /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=3838, host='0.0.0.0')"]

CMD ["/usr/bin/shiny-server"]
