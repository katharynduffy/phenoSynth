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

# df <- read_csv("~/Google Drive/PhenoPostdoc/mapPortal/phenoSites.txt")

# # variables
url <- "https://phenocam.sr.unh.edu/webcam/network/table/"
cams <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="main"]/table') %>%
  html_table()

cams_ = cams[[1]]
site_names = cams_$Camera
layers_ = providers[0:-1]