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


# Downloads file from bundle (whichever ft is set to (only nc and qa_csv))
download_bundle_file = function(site_task_id_, filepath_,  ft){
  print (site_task_id_)
  response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, sep = ""))
  bundle_response = prettify(jsonlite::toJSON(content(response), auto_unbox = TRUE))
  document = jsonlite::fromJSON(txt=bundle_response)
  files = document$files
  
  if (ft == 'nc'){
    netcdf    = subset(files, file_type == 'nc')
    download_this_file = netcdf$file_id
  }else if(ft == 'qa_csv'){
    csvs      = subset(files, file_type == 'csv')
    download_this_file = csvs[grep('Quality-lookup', csvs$file_name), ]$file_id
  }
  print (filepath_)
  response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, '/', download_this_file, sep = ""),
                 write_disk(filepath_, overwrite = TRUE), progress())
  return (filepath_)
}


# Given a site name, function returns the appeears task record
get_appeears_task = function(name, type){
  if (type == 'ndvi'){
    task_pos = grep(name ,appeears_tasks_ndvi$task_name)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks_ndvi[task_pos[i],]$task_name, 3)
      if (row == name){
        task_ = appeears_tasks_ndvi[task_pos[i],]$task_name
        return (subset(appeears_tasks_ndvi, appeears_tasks_ndvi$task_name == task_))
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
  }else {print ('failed to ')}
}