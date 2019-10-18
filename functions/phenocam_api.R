
get_phenocam_camera_df = function(pc_roi_df){
  #get_phenocam_camera_df = function(limit = 2000, pc_roi_df){
  #limit = 2000
  #c      = jsonlite::fromJSON(paste0('https://phenocam.sr.unh.edu/api/cameras/?format=json&limit=',limit))
  # Grab results
  #c = c$results
  # Grab nested metadata from results
  #c_m = c$sitemetadata
  # Bind the two dataframes together
  #c$sitemetadata=NULL
  #cams = cbind(c, c_m)
  # Changing lat/lon/elev from string values into numeric
  #cams[, 2:4] = sapply(cams[, 2:4], as.numeric) 
  cams=phenocamapi::get_phenos()
  # Changing blank values in the camera orientation field to 'N' as a default
  cams$camera_orientation[cams$camera_orientation == ''] = 'N'
  
  roi_sites = unique(pc_roi_df$site)
  num_roi_sites = length(roi_sites)
  
  # Match the two dataframes where the ROI exists
  idx = is.element(cams$site, roi_sites)
  cams_with_rois = cams[idx,]
  # Return cams with roi dataframe
  return (cams_with_rois)
}

get_phenocam_roi_df = function(limit = 2000){
  print ('Grabbing phenocam rois-api')
  roi_files=phenocamapi::get_rois()
  return(roi_files)
}