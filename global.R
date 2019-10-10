# Global file for Shiny App phenoRemote

source('./functions/geospatial.R')
source('./functions/basic.R')
source('./functions/image.R')
source('./functions/leaflet.R')
source('./functions/appeears.R')
source('./functions/data_processing.R')
source('./functions/transition_dates.R')
source('./functions/normalize_ts.R')
source('./functions/gcc_plot.R')
source('./functions/npn_gridded_data.R')
source('./functions/helpers.R')

# For curated dataset
source('./functions/client.R')
library(shinyalert)

bsModalNoClose <-function(...) {
  b = bsModal(...)
  b[[2]]$`data-backdrop` = "static"
  b[[2]]$`data-keyboard` = "false"
  return(b)
}


EMAIL_MODE = FALSE
if (file.exists('./config.R')){
  source('./config.R')
  EMAIL_MODE = TRUE
}

# Libraries
library(shiny)          # Provides web framework for building web applications
library(leaflet)        #
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(lattice)
library(shinyjs)
library(shinyBS)
library(leaflet.extras) #
library(sp)
library(ncdf4)
library(rvest)
library(raster)
library(jsonlite)
library(rgdal)
library(rmarkdown)
library (DT)
library(htmlwidgets)
library(ggplot2)
library(rjson)
library(RCurl)
library(ggthemes)
library(httr)
library(shinycssloaders)
library(data.table)
library(grDevices)
library(plotly)
library(tidyr)

library(knitr)
library(kableExtra)
options(knitr.table.format = "html")
print ('Importing Modules and Phenocam site data')


# Variables

print ('Grabbing phenocam cameras-api')
c      = jsonlite::fromJSON('https://phenocam.sr.unh.edu/api/cameras/?format=json&limit=2000')
c = c$results
c_m=c$sitemetadata
c$sitemetadata=NULL
cams_=cbind(c, c_m)
cams_[is.na(cams_)] = 'N'
cams_[, 2:4] <- sapply(cams_[, 2:4], as.numeric) #changing lat/lon/elev from string values into numeric

print ('Grabbing phenocam rois-api')
rois      = jsonlite::fromJSON('https://phenocam.sr.unh.edu/api/roilists/?format=json&limit=2000')
roi_files = rois$results

idx=is.element(cams_$Sitename, roi_files$site)
cams_=cams_[idx,]

# All site names from table
site_names = cams_$Sitename

# Changing blank values in the camera orientation field to 'N' as a default
cams_$camera_orientation[cams_$camera_orientation == ''] = 'N'

orientation_key = list('N' = 0, 'NE' = 45, 'E' = 90, 'SE' = 135, 'S' = 180, 'SW' = 225, 'W' = 270, 'NW' = 315,
                       'ENE' = 67, 'ESE' = 112, 'NNE' = 22, 'NNW' = 338, 'SSE' = 158, 'SSW' = 202, 'UP' = 0,
                       'WNW' = 292, 'WSW' = 248)
image_sizes_h = list('Small' = 150, 'Medium' = 300, 'Large' = 600)
image_sizes_w = list('Small' = 250, 'Medium' = 500, 'Large' = 1000)


pft_key = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,254,255)
pft_abbreviated = c('Water','EN','EB','DN','DB','MF','SH','SH','SV','SV','GR','WL','AG','UB','MX','TN','UN','NAN','NAN')
pft_expanded = c('Water', 'Evergreen Needleleaf Forest', 'Evergreen Broadleaf Forest', 'Deciduous Needleleaf Forest', 'Deciduous Broadleaf Forest', 'Mixed Forest',
                 'Shrubland', 'Shrubland', 'Woody Savanna', 'Savanna','Grassland', 'Wetland', 'Agriculture', 'Urban', 'Mixed Forest', 'Tundra', 'No Vegetation', 'Unclassified', 'Unclassified' )
pft_df = data.frame(pft_key,pft_abbreviated,pft_expanded)
#insert LandSat classes here
Landsat_Landcover <- read_csv("Landsat.Landcover.csv")

site_filters = c('All', 'Type1', 'Type2', 'Type3', 'NEON', 'Active', 'Inactive')

# Load in dataframes with evi/ndvi for aqua and tera modis data
appeears_tasks_ndvi_tera = readRDS(file = './www/cache_df_ndvi_tera.df')
appeears_tasks_ndvi_aqua = readRDS(file = './www/cache_df_ndvi_aqua.df')
appeears_tasks_evi_tera  = readRDS(file = './www/cache_df_evi_tera.df')
appeears_tasks_evi_aqua  = readRDS(file = './www/cache_df_evi_aqua.df')
appeears_tasks_tds       = readRDS(file = './www/cache_df_tds.df')
appeears_tasks_lc        = readRDS(file = './www/cache_df_lc.df')

#------- added for curated dataset
# Curated dataset sites available in NDVI
cd_client = new("CuratedApiClient", user = 'my_user', password = 'my_pass')
base_url  = baseUrl(cd_client)
login_url = paste0(baseUrl(cd_client), '/login')
services  = sampleServices(cd_client)
# Retrieving a catalog for the curated dataset
catalog = as.data.frame(jsonlite::fromJSON(getCatalog(cd_client)))
catalog
# get unique sites with data to only allow these to display
sites_in_curated_data = unique(catalog$sites.name)
#---------------------------------------------------

# check differences between the landcover and all other cached data.
setdiff(as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6')), as.character(strsplit(appeears_tasks_ndvi_tera$task_name, '_NDVI_v6_tera_sinu')))
setdiff(as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6')), as.character(strsplit(appeears_tasks_ndvi_aqua$task_name, '_NDVI_v6_aqua_sinu')))
setdiff(as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6')), as.character(strsplit(appeears_tasks_evi_tera$task_name, '_EVI_v6_tera_sinu')))    
setdiff(as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6')), as.character(strsplit(appeears_tasks_evi_aqua$task_name, '_EVI_v6_aqua_sinu')))
setdiff(as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6')), as.character(strsplit(appeears_tasks_tds$task_name, '_TDs_v6')))

# Sites with data
# sites_with_data = as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6'))
sites_with_data = unique(as.character(strsplit(appeears_tasks_ndvi_tera$task_name, '_NDVI_v6_tera_sinu')))
curated_sites_with_data = intersect(sites_in_curated_data, sites_with_data)
cams_ = cams_[match(curated_sites_with_data, cams_$Sitename),]
# cams_ = cams_[seq(dim(cams_)[1],1),]

# AppEEARS products page: https://lpdaacsvc.cr.usgs.gov/appeears/products

# defining CRS strings to use for geospatial conversions within the app
sinu_crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
merc_crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
wgs_crs  = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


