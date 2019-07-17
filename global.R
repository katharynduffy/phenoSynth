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

# Load in dataframe with cached AppEEARS tasks
appeears_tasks_ndvi     = readRDS(file = './www/cache_df_ndvi.df')
# Load in df with cached AppEEARS Transition Dates (dts)
appeears_tasks_tds      = readRDS(file = './www/cache_df_tds.df')
# Load in df with cached AppEEARS EVI data
appeears_tasks_evi      = readRDS(file = './www/cache_df_evi.df')

# New
# Load in dataframes with evi/ndvi for aqua and tera modis data
appeears_tasks_ndvi_tera = readRDS(file = './www/cache_df_ndvi_tera.df')
appeears_tasks_ndvi_aqua = readRDS(file = './www/cache_df_ndvi_aqua.df')
appeears_tasks_evi_tera  = readRDS(file = './www/cache_df_evi_tera.df')
appeears_tasks_evi_aqua  = readRDS(file = './www/cache_df_evi_aqua.df')

# AppEEARS products page: https://lpdaacsvc.cr.usgs.gov/appeears/products
