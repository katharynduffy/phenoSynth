# Packages you will need for this shiny app
packages = c('shiny','shinyjs','leaflet','leaflet.extras','httr','tidyr','readr','scales','lattice','jsonlite', 
             'DT', 'shinyBS', 'sp', 'ncdf4', 'rvest', 'ggthemes', 'httr', 'shinycssloaders',
             'data.table', 'grDevices', 'plotly' , 'knitr', 'kableExtra', 'rgdal', 'rjson', 'RCurl')


# Identify new (not installed) packages
new.packages = packages[!(packages %in% installed.packages()[,"Package"])]

# Loop through and download the new packages
if (length(new.packages)[1]==0){
  message('All packages already installed')
}else{
  for (i in 1:length(new.packages)){
    message(paste0('Installing: ', new.packages))
    install.packages(new.packages[i])
  }
}