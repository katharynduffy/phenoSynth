#' show_all_sites
#'
#' @param map_ 
#' @param data_ 
#' add all of these sites back to the leaflet map
#' @return
#' 
show_all_sites = function(map_, data_){
  leafletProxy(map_, data = data_) %>%
    clearMarkers() %>%
    addCircleMarkers(~lon, ~lat, label=~site, layerId=~site, 
                     labelOptions = labelOptions(noHide = F, direction = "bottom", 
                                                 style = get_marker_style()), 
                     opacity = .80, fillColor = get_color(data_), color = get_color(data_),
                     radius = 10, fillOpacity = .20, weight=3.5)
}


#' add_polyline
#'
#' @param datalon_ 
#' @param datalat_ 
#' @param id_ 
#' @param opacity_ 
#' @param color_ 
#' @param map_ 
#' @param group_ 
#' Add a polyline layer to the map
#' 
add_polyline = function(datalon_, datalat_, id_, opacity_, color_='red', map_ = 'map', group_=NULL){
  leafletProxy(map_) %>%
    addPolylines( datalon_,
                  datalat_,
                  layerId = id_,
                  opacity = opacity_,
                  color   = color_,
                  group   = group_)
}


#' add_polygon
#'
#' @param datalon_ 
#' @param datalat_ 
#' @param id_ 
#' @param opacity_ 
#' @param color_ 
#' @param map_ 
#' @param group_ 
#' 
add_polygon = function(datalon_, datalat_, id_, opacity_, color_='red', map_ = 'map', group_ = NULL){
  leafletProxy(map_) %>%
    addPolygons( datalon_,
                 datalat_,
                 layerId = id_,
                 opacity = opacity_,
                 color   = color_,
                 group   = group_)
}


#' remove_polyline
#'
#' @param id_ 
#' @param all 
#' @param map_ 
#' remove all polylines
#' 
remove_polyline = function(id_=NULL, all=TRUE, map_ = 'map'){
  if (all == TRUE){
    leafletProxy(map_) %>%
      clearShapes()
  }else if(all == FALSE){
    leafletProxy(map_) %>% removeShape(layerId = id_)
  }
}

#' get_color
#'
#' @param cams 
#' custom markers created for Active/nonActive
#' 
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
#' get_marker_style
#' Style marker to add to the map when redrawing
#' @return - style
#' 
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


#' zoom_to_site
#'
#' @param site_ 
#' @param site_data_ 
#' @param zoom_ 
#' @param data_ 
#' @param draw_ 
#' @param map_ 
#' @param zoom_value 
#' Zoom to site
#' @return - site data

zoom_to_site = function(site_, site_data_, zoom_, data_, draw_ = NULL, map_ = 'map', zoom_value = 13){
  description        = site_data_$site_description
  camera_orientation = site_data_$camera_orientation
  lat                = site_data_$lat
  lon                = site_data_$lon
  cam_orientation    = as.character(site_data_$camera_orientation)
  
  degrees   = as.numeric(orientation_key[cam_orientation])
  elevation = site_data_$elev
  camera    = site_data_$site
  drawROI   = FALSE
  
  if (zoom_ == TRUE){
    drawROI = draw_
    leafletProxy(map_, data = data_) %>%
      clearPopups() %>%
      clearMarkers() %>%
      # Can add different markers when we zoom in at some point, but for now we will use these circle markers from above
      addCircleMarkers(lng=lon,lat=lat,label=camera, 
                       layerId=camera, 
                       labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                   style = get_marker_style()), 
                       opacity = .80, fillColor = get_color(cams=site_data_), 
                       color = get_color(cams=site_data_),
                       radius = 10, fillOpacity = .20, weight=3.5) %>%
      setView(lng = lon, lat = lat, zoom = zoom_value)
  }
  if (drawROI){
    run_add_polyline(site_data_, degrees)
  }
  return (site_data_)
}


#' get_site_popup
#'
#' @param camera_ 
#' @param lat_ 
#' @param lng_ 
#' @param description_ 
#' @param elevation_ 
#' @param site_type_ 
#' @param camera_orientation_ 
#' @param degrees_ 
#' @param active_ 
#' @param date_end_ 
#' @param date_start_ 
#' @param map_ 
#' Displays the site info when a site is clicked
#' 
get_site_popup <- function(camera_, lat_, lng_, description_, elevation_, site_type_,
                           camera_orientation_, degrees_,
                           active_, date_end_, date_start_,
                           map_ = 'map') {
  website = sprintf('https://phenocam.sr.unh.edu/webcam/sites/%s/', camera_)
  print('Running show a popup box for Site')
  myurl = paste("https://phenocam.sr.unh.edu/data/latest/", camera_, '.jpg', sep = '')
  
  pop = paste0('<div class="leaflet-popup-content">',
               '<h4>','Site name: ', camera_,'</br></h4>',
               '<strong>','lat, long: ','</strong>',lat_,', ', lng_,'</br>',
               '<strong>','Site Description: ','</strong>', description_ ,'</br>',
               '<strong>','Elevation: ','</strong>', elevation_ ,'</br>',
               
               '<strong>','Site type: ','</strong>', site_type_ ,'</br>',
               '<strong>','Orientation (direction): ','</strong>', camera_orientation_ ,'</br>',
               '<strong>','Active: ','</strong>', active_ ,'</br>',
               '<strong>','Start date: ','</strong>', date_start_ ,'</br>',
               '<strong>','End date: ','</strong>', date_end_ ,'</br>',
               
               '<a id="info" href=', website ,' style="text-indent: 0px;"
               class="action-button shiny-bound-input"
               onclick="{Shiny.onInputChange(\'info\', (Math.random() * 1000) + 1);}">',
               
               '<a type="submit" href=', website ,' class="button">Go to Phenocam website</a>'
  )
  
  
  leafletProxy(map_) %>% addPopups(lng_, lat_, popup = pop, layerId = camera_)
}