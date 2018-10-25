# Grabs url for the primary ROI
get_roi_url = function(name, veg_type='None', roi_veg_level='None'){
  roi_url = tryCatch({
    
    if (veg_type=='None'){
      baseurl = 'https://phenocam.sr.unh.edu'
      siteurl = paste('https://phenocam.sr.unh.edu/webcam/sites/',name,'/', sep = '')
      page    = read_html(siteurl)
      html    = page %>% html_nodes("a") %>% html_attr('href')
      html    = grep('data/archive', html, value=TRUE)[[1]]
      html    = paste(baseurl, html, sep='')
      
      page2 = read_html(html)
      html2 = page2 %>% html_nodes("a") %>% html_attr('href')
      html2 = grep('data/archive', html2, value=TRUE)
      html2 = grep('.tif', html2, value=TRUE)
      
      roi     = strsplit(html2, '/')[[1]]
      roi     = grep('.tif', roi, value=TRUE)
      roi     = strsplit(roi, name)[[1]]
      roi     = grep('.tif', roi, value=TRUE)
      roi     = strsplit(roi, '.tif')[[1]]
      roi_url = paste('https://phenocam.sr.unh.edu/data/archive/',
                      name, '/ROI/', name, roi, '_overlay.png', sep = '')
    }else{
      if (roi_veg_level=='Primary'){
        roi_url = paste0(name,'_',veg_type,'_','1000_01_overlay.png')
        roi_url = paste0('https://phenocam.sr.unh.edu/data/archive/',
                         name, '/ROI/', roi_url)
      }else if(roi_veg_level=='Secondary'){
        roi_url = paste0(name,'_',veg_type,'_','0001_01_overlay.png')
        roi_url = paste0('https://phenocam.sr.unh.edu/data/archive/',
                         name, '/ROI/', roi_url)
      }
    }
    return (roi_url)
  },error=function(cond) {message(paste('failed to get roi for sitename:'),isolate(input$site))
    return('Not Found')})
  return (roi_url)
}


# Grabs img url from sitename
get_img_url = function(name){
  url = paste("https://phenocam.sr.unh.edu/data/latest/", name, ".jpg",sep = '')
  return (url)
}