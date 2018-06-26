# Libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(lattice)
library(geojsonio)
library(shinyjs)
library(leaflet.extras)
library(sp)
library(rvest)

# Variables
table_url = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/?format=csv'
df <- read.csv(url(table_url))
colnames(df)

layers_ = providers[0:-1]
# Data for all the sites
cams_ = df
# All site names from table
site_names = df$site

# Changing blank values in the camera orientation field to 'N' as a default
cams_$camera_orientation[cams_$camera_orientation == ''] = 'N'

orientation_key = list('N' = 0, 'NE' = 45, 'E' = 90, 'SE' = 135, 'S' = 180, 'SW' = 225, 'W' = 270, 'NW' = 315,
                       'ENE' = 67, 'ESE' = 112, 'NNE' = 22, 'NNW' = 338, 'SSE' = 158, 'SSW' = 202, 'UP' = 0,
                       'WNW' = 292, 'WSW' = 248)

# orientation = as.character(cams_$camera_orientation[5])
# orientation_key[orientation]
  

# Variables being used in code :
#   site = Name of the phenocam site
#   lat/lon = location of phenocam site
#   elev = elevation of the site
#   site_type = I, II, or III
#   camera_orientation = direction the camera is pointing
#   site_description = information about the phenocam site
#   nimage = Number of images
#   camera_description = Name of the camera / other
#   active = bool (Active or Not)
#   date_end = last date images collected on
#   date_start = first date images collected on

# is not null function
is.not.null <- function(x) ! is.null(x)


