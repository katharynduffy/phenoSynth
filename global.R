# Global file for Shiny App phenoRemote


# Libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(RColorBrewer)
library(scales)
library(lattice)
library(shinyjs)
library(leaflet.extras)
library(sp)
library(rvest)
library(raster)
# library(rgdal)
library (DT)
library(htmlwidgets)
library(ggplot2)

if(!require(devtools)){install.packages("devtools")}
devtools::install_github("khufkens/MODISTools", build_vignettes = FALSE)
library(MODISTools)

# Variables
table_url = 'https://phenocam.sr.unh.edu/webcam/network/siteinfo/?format=csv'
df <- read.csv(url(table_url))
colnames(df)
#layers_ = providers[0:-1]
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

pft_key=list(0='Water', 1='EN', 2='EB', 3='DN', 4='DB', 5='MF', 6='SH', 7='SH', 8='SV', 9='SV', 10='GR', 11='WL', 12='AG', 13='UB', 14='MX', 15='TN', 16='UN', 254='NAN', 255='NAN')

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
