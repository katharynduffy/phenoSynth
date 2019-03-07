FROM rocker/shiny:latest

# Change permissions for R library
RUN chmod -R a+rwx /usr/local/lib/R/site-library

# Install Linux dependencies
RUN apt-get update && apt-get install -y curl git libboost-all-dev \
    libgdal-dev libgdal20 libgit2-dev libproj-dev libssl-dev  \
    libudunits2-0 libudunits2-dev
    
# Install R Packages w/ Dependencies    
RUN R -e "install.packages(c('curl', 'devtools', 'digest', 'dplyr', 'DT', \
    'geosphere', 'ggplot2', 'ggthemes', 'glue', 'httr', 'htmlwidgets', \
    'htmltools', 'httpuv', 'jsonlite', 'kableExtra', 'lattice','leaflet', \
    'leaflet.extras', 'magrittr', 'markdown', 'mime', 'plyr', 'readr', \
    'RCurl', 'Rcpp', 'reshape2', 'rgdal', 'rjson','phenoSynth', 'plotly', \
    'scales', 'shiny', 'shinyjs','shinythemes', 'shinycssloaders', \
    'shinyWidgets', 'shinyBS', 'sf', 'stringi', 'tidyr', \
    'units', 'yaml'), dependencies=TRUE)"

# Remove Shiny example inherited from the base image
RUN rm -rf /srv/shiny-server/*

RUN cd /srv/shiny-server/

# Copy the Source Code of the app into the container
# COPY Server.R /srv/shiny-server/
# COPY Ui.R /srv/shiny-server/

# change permission of the shiny folder where the app sits
RUN chmod -R 777 /srv/shiny-server

# Start the server with the container
CMD ["/usr/bin/shiny-server.sh"]