# Returns site name from a cached task
# name_length :  the length of most task names submitted.  kelloggswitchgrass_09_06_18_0839 would be name_length = 5
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


# Downloads full bundle of data from AppEEARS for site_task_id_
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

# Returns a AppEEARS task id bundle dataframe
get_appeears_bundle_df = function(site_task_id_){
  response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, sep = ""))
  bundle_response = jsonlite::toJSON(content(response), auto_unbox = TRUE)
  document = jsonlite::fromJSON(txt=bundle_response)
  files = document$files
  return (files)
}


# Given a site name, function returns the appeears task record
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
    return (subset(appeears_tasks_lc, as.character(strsplit(appeears_tasks_lc$task_name, '_LC_nc_v6')) == name))
  }else {print (paste0('failed to grab task: ',name , ', ', type))}
}