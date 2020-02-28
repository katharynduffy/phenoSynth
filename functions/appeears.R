#' get_site_from_task
#'
#' @param task_name_ 
#' @param name_length - the length of most task names submitted.  kelloggswitchgrass_09_06_18_0839 would be name_length = 5
#'
#' @return# site name from a cached task
#' 
get_site_from_task = function(task_name_, name_length){
  elements = strsplit(task_name_, split = '_', fixed=TRUE)
  element_length = length(elements[[1]])
  if (element_length == name_length){
    site_name_ = elements[[1]][1]
    return (site_name_)
  }else if(element_length > name_length){
    num = element_length - name_length
    elem = elements[[1]][1]
    for (x in c(1:num)){
      elem = paste(elem, elements[[1]][x+1], sep='_')
    }
    return (elem)
  }else{
    sprintf('This task is missing information/invalid: %s', task_name_)
    return(FALSE)
  }
}


#' download_bundle_file
#'Downloads full bundle of data from AppEEARS for site_task_id_
#' @param site_task_id_ 
#' @param filepath_ - the path of a file
#'
#' @return Return dataframe of files
download_bundle_file = function(site_task_id_, filepath_){
  files = get_appeears_bundle_df(site_task_id_)
  # Loop through all files in document and download
  for (file in files$file_id){
    download_this_file = file
    filename = subset(files, files$file_id == file)$file_name
    # create a destination directory to store the file in
    filepath = paste(filepath_, filename, sep = '')
    print (filepath)
    # write the file to disk using the destination directory and file name
    response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, '/', download_this_file, sep = ""),
                   write_disk(filepath, overwrite = TRUE), progress())
  }
  # Return dataframe of files
  return (files)
}

#' get_appeears_bundle_df
#'
#' @param site_task_id_ 
#'
#' @return Returns a AppEEARS task id bundle dataframe
#' 
get_appeears_bundle_df = function(site_task_id_){
  response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, sep = ""))
  bundle_response = jsonlite::toJSON(content(response), auto_unbox = TRUE)
  document = jsonlite::fromJSON(txt=bundle_response)
  files = document$files
  return (files)
}


#' get_appeears_task
#'
#' @param name - site name
#' @param type - site type
#'
#' @return the appeears task record
#' 
get_appeears_task = function(name, type){
  if (type == 'ndvi_tera'){
    return (subset(appeears_tasks_ndvi_tera, as.character(strsplit(appeears_tasks_ndvi_tera$task_name, '_NDVI_v6_tera_sinu')) == name))
  }else if (type == 'ndvi_aqua'){
    return (subset(appeears_tasks_ndvi_aqua, as.character(strsplit(appeears_tasks_ndvi_aqua$task_name, '_NDVI_v6_aqua_sinu')) == name))
  }else if (type == 'tds'){
    return (subset(appeears_tasks_tds, as.character(strsplit(appeears_tasks_tds$task_name, '_TDs_v6')) == name))
  }else if (type == 'evi_tera'){
    return (subset(appeears_tasks_evi_tera, as.character(strsplit(appeears_tasks_evi_tera$task_name, '_EVI_v6_tera_sinu')) == name))
  }else if (type == 'evi_aqua'){
    return (subset(appeears_tasks_evi_aqua, as.character(strsplit(appeears_tasks_evi_aqua$task_name, '_EVI_v6_aqua_sinu')) == name))
  }else if (type == 'landcover'){
    return (subset(appeears_tasks_lc, as.character(strsplit(appeears_tasks_lc$task_name, '_LC_sinu_v6')) == name))
  }else {print (paste0('failed to grab task: ',name , ', ', type))}
}


#' get_submitions_remaining
#'
#'
#' @return Returns the number of submitions remaining for AppEEARS tasks
#' 
get_submitions_remaining = function(appeears_tasks){
  # Get current time in POSIXct
  now = Sys.time()
  appeears_tasks$created = as.POSIXct(appeears_tasks$created,format="%Y-%m-%dT%H:%M", tz = 'UTC')
  attributes(appeears_tasks$created)$tzone = 'MST'
  
  # Grab tasks created in the last 24 hours
  last_24h_tasks = subset(appeears_tasks, appeears_tasks$created > (now - (60*60*24*1)))
  
  # Number of submitions remaining for the last 24hours
  n_tasks_24h = dim(last_24h_tasks)[1]
  submitions_remaining = 100 - n_tasks_24h
  
  return (submitions_remaining)
}
