# add all of these sites back to the leaflet map
show_all_sites = function(map_, data_){
  leafletProxy(map_, data = data_) %>%
    clearMarkers() %>%
    addCircleMarkers(~Lon, ~Lat, label=~Sitename, layerId=~Sitename, 
                     labelOptions = labelOptions(noHide = F, direction = "bottom", 
                                                 style = get_marker_style()), 
                     opacity = .80, fillColor = get_color(data_), color = get_color(data_),
                     radius = 10, fillOpacity = .20, weight=3.5)
}


# Add a polyline layer to the map
add_polyline = function(datalon_, datalat_, id_, opacity_, color_='red', map_ = 'map', group_=NULL){
  leafletProxy(map_) %>%
    addPolylines( datalon_,
                  datalat_,
                  layerId = id_,
                  opacity = opacity_,
                  color   = color_,
                  group   = group_)
}


add_polygon = function(datalon_, datalat_, id_, opacity_, color_='red', map_ = 'map', group_ = NULL){
  leafletProxy(map_) %>%
    addPolygons( datalon_,
                 datalat_,
                 layerId = id_,
                 opacity = opacity_,
                 color   = color_,
                 group   = group_)
}


# remove all polylines
remove_polyline = function(id_=NULL, all=TRUE, map_ = 'map'){
  if (all == TRUE){
    leafletProxy("map") %>%
      clearShapes()
  }else if(all == FALSE){
    leafletProxy(map_) %>% removeShape(layerId = id_)
  }
}

# custom markers created for Active/nonActive
get_color <- function(cams) {
  sapply(cams$active, function(active) {
    if(active == 'TRUE') {
      "blue"
    } else if(active == 'FALSE') {
      "red"
    } else {
      "orange"
    } })
}

# Style marker to add to the map when redrawing
get_marker_style = function(){
  style = list(
    "color" = "black",
    "font-family" = "serif",
    # "font-style" = "italic",
    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    "font-size" = "14px",
    "border-color" = "rgba(0,0,0,0.5)"
  )
  return(style)
}