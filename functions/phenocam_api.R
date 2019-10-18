
get_phenocam_camera_df = function(pc_roi_df){
  # Use the phenocamapi package to pull in the phenocams data
  cams = phenocamapi::get_phenos()
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