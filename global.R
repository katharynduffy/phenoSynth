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
library(birk)
library(rnpn)

# Function for Login Model
bsModalNoClose = function(...) {
  b = bsModal(...)
  b[[2]]$`data-backdrop` = "static"
  b[[2]]$`data-keyboard` = "false"
  return(b)
}

# Set knitr table format
options(knitr.table.format = "html")

# Phenocam site data, that will be cached daily
print ('Importing Modules and Phenocam site data')

# Cache out the cams_ and rois_ dataframes from phenocam api
pc_data   = cache_phenocam_data()
cams_     = pc_data$cams
roi_files = pc_data$rois
# Removes old cached phenocam data older than 30 days
#remove_old_cached_phenocam_data(days_back = 30)

# Build crosswalk b
site_names = cams_$site
orientation_key = list('N' = 0, 'NE' = 45, 'E' = 90, 'SE' = 135, 'S' = 180, 'SW' = 225, 'W' = 270, 'NW' = 315,
                       'ENE' = 67, 'ESE' = 112, 'NNE' = 22, 'NNW' = 338, 'SSE' = 158, 'SSW' = 202, 'UP' = 0,
                       'WNW' = 292, 'WSW' = 248)
pft_key = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,253,254,255)
pft_abbreviated = c('Water','EN','EB','DN','DB','MF','SH','SH','SV','SV','GR','WL','AG','UB','MX','TN','UN','NV','NAN','NAN')
pft_expanded = c('Water', 'Evergreen Needleleaf Forest', 'Evergreen Broadleaf Forest', 'Deciduous Needleleaf Forest', 
                 'Deciduous Broadleaf Forest', 'Mixed Forest','Shrubland', 'Shrubland', 'Woody Savanna', 'Savanna',
                 'Grassland', 'Wetland', 'Agriculture', 'Urban', 'Mixed Forest', 'Tundra', 'Unknown', 'No Vegetation', 
                 'Unclassified', 'Unclassified')
pft_df = data.frame(pft_key,pft_abbreviated,pft_expanded, stringsAsFactors=FALSE)

# Insert LandSat classes here
Landsat_Landcover = read_csv("./www/Landsat.Landcover.csv") # updated landsat landclass so that we just reference this table
site_filters = c('All', 'Type1', 'Type2', 'Type3', 'NEON', 'Active', 'Inactive')

# AppEEARS products page: https://lpdaacsvc.cr.usgs.gov/appeears/products
# Load in AppEEARS lookup dataframes for all data types
appeears_tasks_ndvi_tera = readRDS(file = './www/cache_df_ndvi_tera.df')
appeears_tasks_ndvi_aqua = readRDS(file = './www/cache_df_ndvi_aqua.df')
appeears_tasks_evi_tera  = readRDS(file = './www/cache_df_evi_tera.df')
appeears_tasks_evi_aqua  = readRDS(file = './www/cache_df_evi_aqua.df')
appeears_tasks_tds       = readRDS(file = './www/cache_df_tds.df')
appeears_tasks_lc        = readRDS(file = './www/cache_df_lc.df')

# QC values for MODIS data
qc_df_tera = read.csv('./www/MOD13Q1-006-250m-16-days-VI-Quality-lookup.csv',stringsAsFactors = FALSE)
qc_df_aqua = read.csv('./www/MYD13Q1-006-250m-16-days-VI-Quality-lookup.csv',stringsAsFactors = FALSE)
qc_vals=read.csv('./www/qc_vals.csv', stringsAsFactors = FALSE)
names(qc_vals)[1]='Value'
# qc_vals=qc_vals%>%
#   filter(Aerosol.Quantity== 'Low')


#Allow for email mode (depreciated but still useful)
EMAIL_MODE = FALSE
if (file.exists('./config.R')){
  source('./config.R')
  EMAIL_MODE = TRUE
}

# defining CRS strings to use for geospatial conversions within the app
sinu_crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
merc_crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
wgs_crs  = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
alb_crs  = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
