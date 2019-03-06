#--------------------------------------------------------------------------------------------------------------------------------------

FROM openanalytics/r-base

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl-dev

# RUN add-apt-repository -y ppa:opencpu/jq

# system library dependency for the phenocam app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    libgit2-dev \
    libprotobuf-dev \
    libxml2-dev \
    libmagick++-dev \
    libv8-3.14-dev \
    gdebi-core 

# Shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the phenocam app
RUN R -e "install.packages(c('leaflet','dplyr','magick','readr','RColorBrewer','scales','lattice','shinyjs','leaflet.extras','sp','rvest','raster','DT','htmlwidgets'), repos='https://cloud.r-project.org/')"

# shiny examples     
RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

## Uncomment the line below to include a custom configuration file. You can download the default file at
## https://raw.githubusercontent.com/rstudio/shiny-server/master/config/default.config
## (The line below assumes that you have downloaded the file above to ./shiny-customized.config)
## Documentation on configuration options is available at
## http://docs.rstudio.com/shiny-server/

# COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server.sh"]


#--------------------------------------------------------------------------------------------------------------------------------------