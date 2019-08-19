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
  bundle_response = prettify(jsonlite::toJSON(content(response), auto_unbox = TRUE))
  document = jsonlite::fromJSON(txt=bundle_response)
  files = document$files
  return (files)
}


# Given a site name, function returns the appeears task record
get_appeears_task = function(name, type){
  if (type == 'ndvi_tera'){
    task_pos = grep(name, appeears_tasks_ndvi_tera$task_name)
    print (task_pos)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks_ndvi_tera[task_pos[i],]$task_name, 5)
      if (row == name){
        task_ = appeears_tasks_ndvi_tera[task_pos[i],]$task_name
        return (subset(appeears_tasks_ndvi_tera, appeears_tasks_ndvi_tera$task_name == task_))
      }
    }
  }else if (type == 'ndvi_aqua'){
    task_pos = grep(name, appeears_tasks_ndvi_aqua$task_name)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks_ndvi_aqua[task_pos[i],]$task_name, 5)
      if (row == name){
        task_ = appeears_tasks_ndvi_aqua[task_pos[i],]$task_name
        return (subset(appeears_tasks_ndvi_aqua, appeears_tasks_ndvi_aqua$task_name == task_))
      }
    }
  }else if (type == 'tds'){
    task_pos = grep(name, appeears_tasks_tds$task_name)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks_tds[task_pos[i],]$task_name, 3)
      if (row == name){
        task_ = appeears_tasks_tds[task_pos[i],]$task_name
        return (subset(appeears_tasks_tds, appeears_tasks_tds$task_name == task_))
      }
    }
  }else if (type == 'evi_tera'){
    task_pos = grep(name, appeears_tasks_evi_tera$task_name)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks_evi_tera[task_pos[i],]$task_name, 5)
      if (row == name){
        task_ = appeears_tasks_evi_tera[task_pos[i],]$task_name
        return (subset(appeears_tasks_evi_tera, appeears_tasks_evi_tera$task_name == task_))
      }
    }
  }else if (type == 'evi_aqua'){
    task_pos = grep(name, appeears_tasks_evi_aqua$task_name)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks_evi_aqua[task_pos[i],]$task_name, 5)
      if (row == name){
        task_ = appeears_tasks_evi_aqua[task_pos[i],]$task_name
        return (subset(appeears_tasks_evi_aqua, appeears_tasks_evi_aqua$task_name == task_))
      }
    }
  }else {print (paste0('failed to grab task: ',name , ', ', type))}
}