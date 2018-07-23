# Global file for Shiny App phenoRemote


# Libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(lattice)
# library(geojsonio)
library(shinyjs)
library(leaflet.extras)
library(sp)
library(rvest)
library(raster)
# library(magick)
# library(rgdal)
library (DT)
library(htmlwidgets)

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
image_sizes_h = list('Small' = 150, 'Medium' = 300, 'Large' = 600)
image_sizes_w = list('Small' = 250, 'Medium' = 500, 'Large' = 1000)

site_filters = c('All', 'Type1', 'Type2', 'Type3', 'NEON', 'Active', 'Inactive')

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

# custom markers created for Active/nonActive
getColor <- function(cams) {
  sapply(cams$active, function(active) {
    if(active == 'True') {
      "blue"
    } else if(active == 'False') {
      "red"
    } else {
      "orange"
    } })
}

##### Gives us the option to add our own images from our computer or a website.
#####   Leaving here for now.
# # Choose Icon:
# leafIcons <- icons(
#   iconUrl = ifelse(cams_$active == 'True',
#                    "/users/kenns/downloads/grass.png",
#                    "http://leafletjs.com/docs/images/leaf-red.png"
#   ),
#   iconWidth = 38, iconHeight = 95,
#   iconAnchorX = 22, iconAnchorY = 94)
# html_legend <- "<img src='http://leafletjs.com/docs/images/leaf-green.png'>green<br/>
# <img src='http://leafletjs.com/docs/images/leaf-red.png'>red"
