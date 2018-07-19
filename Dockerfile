FROM openanalytics/r-base

MAINTAINER Kyle Enns "kenns@usgs.gov"

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
    libssl1.0.0

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the superzip app
RUN R -e "install.packages(c('leaflet','dplyr','readr','RColorBrewer','scales','lattice','geojsonio','shinyjs','leaflet.extras','sp','rvest','raster','magick','rgdal','DT','htmlwidgets')), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/app
COPY app /root/app

# COPY Rprofile.site /usr/lib/R/etc/  # I don't think I need this for phenoremote app

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/root/app')"]


#----------------------------------------------------------

# # Starting image to build from
# FROM rocker/shiny:latest
# MAINTAINER Kyle Enns (kenns@usgs.gov)

# # Installing dependencies
# RUN apt-get update && apt-get install -y \
#     gnupg2 libssl-dev \
#     && apt-get clean \ 
#     && rm -rf /var/lib/apt/lists/ \ 
#     && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# # # Install packages from CRAN (and clean up)
# # RUN Rscript -e "install.packages(c('leaflet','shiny','dplyr','readr','RColorBrewer','scales','lattice','geojsonio','shinyjs','leaflet.extras','sp','rvest','raster','magick','rgdal','DT','htmlwidgets'), repos='https://cran.rstudio.com/')" \
# #     && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# COPY . /srv/shiny-server/phenoremote/

# EXPOSE 3838