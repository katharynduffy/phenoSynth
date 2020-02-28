#' switch_to_explorer_panel
#' @param map_ 
#' Switch to explorer panel
#' 
switch_to_explorer_panel = function(map_ = 'map'){
  shiny::hideTab('navbar', 'PlotPanel')
  shiny::hideTab('navbar', 'paoiTab')
  # Ids to hide:
  shinyjs::hide(id = 'analyzerTitle')
  shinyjs::hide(id = 'siteExplorerMode')
  shinyjs::hide(id = 'showModisSubset')
  shinyjs::hide(id = 'drawROI')
  shinyjs::hide(id = 'azm')
  shinyjs::hide(id = 'siteTitle')
  shinyjs::hide(id = 'plotRemoteData')
  shinyjs::hide(id = 'pftSelection')
  shinyjs::hide(id = 'showHidePlot')
  shinyjs::hide(id = 'modisLegend')
  shinyjs::hide(id = 'plotpanel')
  shinyjs::hide(id = 'highlightPixelModeNDVI')
  shinyjs::hide(id = 'plotPixelsNDVI')
  shinyjs::hide(id = 'getData')
  shinyjs::hide(id = 'getDataPopup')
  shinyjs::hide(id = 'clearPixels')
  shinyjs::hide(id = 'uploadShp')
  shinyjs::hide(id = 'nlcdOpacity')
  shinyjs::hide(id = 'openDeleteDataModal')
  # Ids to show:
  shinyjs::show(id = 'controls')
  shinyjs::show(id = 'explorerTitle')
  shinyjs::show(id = 'usZoom')
  shinyjs::show(id = 'showSites')
  shinyjs::show(id = 'filterSites')
  shinyjs::show(id = 'site')
  shinyjs::show(id = 'siteZoom')
  shinyjs::show(id = 'drawImage')
  shinyjs::show(id = 'drawImageROI')
  shinyjs::show(id = 'analyzerMode')
  shinyjs::show(id = 'mouse')
  
  # Create leaflet map for explorer mode
  leafletProxy(map_) %>%
    clearControls() %>%
    clearShapes() %>%
    clearImages() %>%
    addLegend(values = c(1,2), group = "site_markers", position = "bottomright", title = 'Phenocam Activity',
              labels = c("Active sites", "Inactive sites"), colors= c("blue","red")) %>%
    addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                     position = c("topleft"),
                     options = layersControlOptions(collapsed = TRUE))
}

#' switch_to_analyzer_panel
#' Switches the main panel over to analyzer mode
#' 
switch_to_analyzer_panel = function(){
  # Ids to show:
  shinyjs::show(id = 'analyzerTitle')
  shinyjs::show(id = 'siteExplorerMode')
  shinyjs::show(id = 'showModisSubset')
  shinyjs::show(id = 'drawROI')
  shinyjs::show(id = 'drawImage')
  shinyjs::show(id = 'drawImageROI')
  shinyjs::show(id = 'mouse')
  shinyjs::show(id = 'siteTitle')
  shinyjs::show(id = 'pftSelection')
  shinyjs::show(id = 'getData')
  shinyjs::show(id = 'uploadShp')
  shinyjs::show(id = 'openDeleteDataModal')
  # Ids to hide:
  shinyjs::hide(id = 'explorerTitle')
  shinyjs::hide(id = 'usZoom')
  shinyjs::hide(id = 'showSites')
  shinyjs::hide(id = 'analyzerMode')
  shinyjs::hide(id = 'filterSites')
  shinyjs::hide(id = 'site')
  shinyjs::hide(id = 'siteZoom')
  shinyjs::hide(id = 'showHidePlot')
  shinyjs::hide(id = 'plotRemoteData')
  shinyjs::hide(id = 'doneGetData')
  shinyjs::hide(id = 'clearPixels')
}


# is not null function
is_not_null = function(x) ! is.null(x)


# not in function
'%!in%' = function(x, y) {
  ! ('%in%'(x, y))
}

#' get_download_folder
#'
#' @return Downloads folder for windows/macos
#' 
get_download_folder = function(){
  if (Sys.info()['sysname'] == 'Darwin'){
    folder = paste('/Users/', Sys.getenv('LOGNAME'),'/Downloads/', sep = '')
  }else if (Sys.info()['sysname'] == 'Windows'){
    folder = paste('C:/Downloads/', sep = '')
  }else{
    folder = ''
  }
  return (folder)
}

#' delete_file
#'
#' @param filepath_ 
#' Deletes any file at filepath_ (does not delete directories)
#' 
delete_file = function(filepath_){
  if (file.exists(filepath_)) file.remove(filepath_)
}


#' get_custom_color_list
#'
#' @param len - length of list
#'
#' @return a vector with different RGB values
#' 
get_custom_color_list = function(len){
  colors = c()
  len = len +2
  g = 0
  b = 0
  val = as.integer(255/(len/3))
  r = -(val)
  for (x in c(1:len)){
    
    if ((x+2)%%3==0){
      r = val + r
    }
    else if ((x+1)%%3==0){
      g = val + g
    }
    else if (x%%3==0){
      b = val + b
    }
    colors = c(colors, paste0('rgb(',r,',',g,',',b,')'))
  }
  return(colors[2:(len-1)])
}