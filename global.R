# Global file for Shiny App

# Import Local Functions
source('install.R')
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
source('./functions/phenocam_api.R')

EMAIL_MODE = FALSE
if (file.exists('./config.R')){
  source('./config.R')
  EMAIL_MODE = TRUE
}

# Libraries
library(shiny)    
library(leaflet)      
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(lattice)
library(shinyjs)
library(shinyBS)
library(leaflet.extras)
library(sp)
library(ncdf4)
library(rvest)
library(raster)
library(jsonlite)
library(rgdal)
library(rmarkdown)
library(DT)
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
library(raster)
library(knitr)
library(kableExtra)
library(shinyalert)
library(phenocamapi)
library(changepoint)

# Set knitr table format
options(knitr.table.format = "html")


# Add catch here to ping phenocam surver and make sure it is up and running.
#  if not, bring in most up to date version of cams and rois cached (if available)

# Phenocam site data, that will be cached daily 
print ('Importing Modules and Phenocam site data')
todays_date = Sys.Date()
todays_cams_name = paste0('./www/phenocam_data/cams_', todays_date)
todays_rois_name = paste0('./www/phenocam_data/rois_', todays_date)

# Create directory to store phenocam data if it doesn't exist
if(dir.exists('./www/phenocam_data')==FALSE){
  dir.create('./www/phenocam_data')
  roi_files = get_phenocam_roi_df()
  cams_     = get_phenocam_camera_df(pc_roi_df = roi_files) %>% dplyr::select(-c('flux_networks'))
  write.csv(roi_files, todays_rois_name)
  write.csv(cams_, todays_cams_name)
} else{
  # If directory exists check to see if todays cams and rois exist
  if (file.exists(todays_cams_name) & file.exists(todays_rois_name)){
    cams_ = read.csv(todays_cams_name)
    roi_files = read.csv(todays_rois_name)
  }else {
    roi_files = get_phenocam_roi_df()
    cams_     = get_phenocam_camera_df(pc_roi_df = roi_files) %>% dplyr::select(-c('flux_networks'))
    write.csv(roi_files, todays_rois_name)
    write.csv(cams_, todays_cams_name)
  }
}

# All site names from table
site_names = cams_$site
orientation_key = list('N' = 0, 'NE' = 45, 'E' = 90, 'SE' = 135, 'S' = 180, 'SW' = 225, 'W' = 270, 'NW' = 315,
                       'ENE' = 67, 'ESE' = 112, 'NNE' = 22, 'NNW' = 338, 'SSE' = 158, 'SSW' = 202, 'UP' = 0,
                       'WNW' = 292, 'WSW' = 248)

pft_key = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,254,255)
pft_abbreviated = c('Water','EN','EB','DN','DB','MF','SH','SH','SV','SV','GR','WL','AG','UB','MX','TN','UN','NAN','NAN')
pft_expanded = c('Water', 'Evergreen Needleleaf Forest', 'Evergreen Broadleaf Forest', 'Deciduous Needleleaf Forest', 'Deciduous Broadleaf Forest', 'Mixed Forest',
                 'Shrubland', 'Shrubland', 'Woody Savanna', 'Savanna','Grassland', 'Wetland', 'Agriculture', 'Urban', 'Mixed Forest', 'Tundra', 'No Vegetation', 'Unclassified', 'Unclassified' )
pft_df = data.frame(pft_key,pft_abbreviated,pft_expanded)
# Insert LandSat classes here
Landsat_Landcover = read_csv("./www/Landsat.Landcover.csv") # updated landsat landclass so that we just reference this table

site_filters = c('All', 'Type1', 'Type2', 'Type3', 'NEON', 'Active', 'Inactive')

# Load in AppEEARS lookup dataframes for all data types
appeears_tasks_ndvi_tera = readRDS(file = './www/cache_df_ndvi_tera.df')
appeears_tasks_ndvi_aqua = readRDS(file = './www/cache_df_ndvi_aqua.df')
appeears_tasks_evi_tera  = readRDS(file = './www/cache_df_evi_tera.df')
appeears_tasks_evi_aqua  = readRDS(file = './www/cache_df_evi_aqua.df')
appeears_tasks_tds       = readRDS(file = './www/cache_df_tds.df')
appeears_tasks_lc        = readRDS(file = './www/cache_df_lc.df')

# AppEEARS products page: https://lpdaacsvc.cr.usgs.gov/appeears/products

# defining CRS strings to use for geospatial conversions within the app
sinu_crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
merc_crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
wgs_crs  = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
