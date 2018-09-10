FROM rocker/binder:3.5.0

# Copy repo into ${HOME}, make user own $HOME
USER root

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils

RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
  unixodbc-dev \ 
  libcurl4-openssl-dev \
  libssl-dev \
  zlib1g-dev \
  libpng-dev \
  libxml2-dev \ 
  tcl tk tk-dev\
  pandoc \
  libx11-dev \ 
  libglu1-mesa-dev mesa-common-dev \
  libfreetype6-dev \
  libcairo2-dev \
  libsqlite-dev \
  libmariadbd-dev \
  libmariadb-client-lgpl-dev \
  libpq-dev \
  libssh2-1-dev \
  libmagick++-dev \
  libudunits2-dev \
  git-core \
  vim \
  software-properties-common \ 
  && apt-get -y --no-install-recommends install gdal-bin libgdal-dev \
  && R -e "source('https://bioconductor.org/biocLite.R')" \
  && install2.r --error \
    --deps TRUE \
    RColorBrewer \
    dplyr \
    ggplot2 \
    ggthemes \
    httr \
    jsonlite \
    leaflet \
    leaflet.extras \
    pkgconfig \
    raster \
    rgeos \
    rgdal \
    rlang \
    rjson \
    rmarkdown \
    rvest \
    scales \
    shiny \
    shinyjs \
    sp

COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

## run any install.R script we find
RUN if [ -f install.R ]; then R --quiet -f install.R; fi