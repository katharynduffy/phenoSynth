# Packages you will need for this shiny app
packages = c('shiny','shinyjs','leaflet','leaflet.extras','httr','tidyr','readr','scales','lattice','jsonlite', 
             'DT', 'shinyBS', 'sp', 'ncdf4', 'rvest', 'ggthemes', 'httr', 'shinycssloaders', 'changepoint', 'rnpn',
             'data.table', 'grDevices', 'plotly' , 'knitr', 'kableExtra', 'rgdal', 'rjson', 'RCurl', 'phenocamapi',
             'birk', 'devtools', 'AppEEARS4R')


# Identify new (not installed) packages
new.packages = packages[!(packages %in% installed.packages()[,"Package"])]

# Loop through and download the new packages
if (length(new.packages)[1]==0){
  message('All packages already installed')
}else{
  for (i in 1:length(new.packages)){
    message(paste0('Installing: ', new.packages))
    if (new.packages[i] == 'AppEEARS4R'){
      library(devtools)
      devtools::install_github("katharynduffy/AppEEARS4R")
    }else {
      install.packages(new.packages[i])
    }
  }
}