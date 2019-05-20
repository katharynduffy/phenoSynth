# Grabs img url from sitename
get_img_url = function(name){
  url = paste("https://phenocam.sr.unh.edu/data/latest/", name, ".jpg",sep = '')
  return (url)
}

# grab url for roi from phenocam using name of site and abbreviated PFT
get_roi_url = function(name, pft_abr){
  
  roi_url = tryCatch({
  
    grep_roi = paste0(pft_abr, '_1000_01')
    url = paste0('https://phenocam.sr.unh.edu/data/archive/', name, '/ROI/')
    page = read_html(url)
    html = page %>% html_nodes('a') %>% html_attr('href')
    pngs = html[grep('.png', html)]
    roi_name = pngs[grep(grep_roi, pngs)]
    roi_url  = paste0('https://phenocam.sr.unh.edu/data/archive/',
                      name, '/ROI/', roi_name)
  return(roi_url)
  },error=function(cond) {message(paste('failed to get roi for sitename:'),name)
    return('Not Found')})
}