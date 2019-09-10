#packages you will need for this shiny app
packages = c('shiny','shinyjs','leaflet','leaflet.extras','httr','tidyr','readr','scales','lattice','jsonlite', 'DT', 'shinyBS', 'leaflet.extras', 'sp', 'ncdf4', 'rvest', 'ggthemes', 'httr', 'shinycssloaders','data.table', 'grDevices', 'plotly' , 'knitr', 'kableExtra', 'rgdal', 'rjson')
for (i in 1:length(packages))
{
  install.packages(packages[i])
}


