#' download_npn_brick
#'
#' @param tmp_name 
#' @param out_file 
#' @param layer 
#' @param number_of_years 
#' Function that builds and saves out a Raster brick for a specific layer from npn
#' @return - geoserver url
#' 
download_npn_brick = function(tmp_name,
                              out_file,
                              layer = 'si-x:average_leaf_prism',
                              number_of_years = 18){
  dates = c()
  files_ = c()
  average_leaf_prism_brick = raster::brick()
  for (x in c(1:number_of_years)){
    year = 2001 + x - 1
    date = paste0(as.character(year),'-12-31')
    dates = c(dates, date)
    file_ = paste0(tmp_name, date)
    files_ = c(files_, file_)
    print (file_)
    #4k Resolution <https://docs.google.com/document/d/1jDqeh8k30t0vEBAJu2ODiipaofLZ2PFgsaNzhhzz3xg/pub>
    npn_download_geospatial_(layer, date, output_path = file_)
    this_date_raster = raster::raster(file_)
    average_leaf_prism_brick = raster::addLayer(average_leaf_prism_brick, this_date_raster)
  }
  
  #save out brick
  npn_brick = raster::writeRaster(average_leaf_prism_brick, out_file, 
                                  format = 'CDF', overwrite=TRUE)
  for (f in files_){
    file.remove(f)
  }
  return (npn_brick)
}

npn_download_geospatial_ = function (coverage_id, date, format = "geotiff", output_path = NULL) 
{
  z = NULL
  if (is.null(output_path)) {
    z = tempfile()
  }
  s = "&SUBSET="
  param = tryCatch({
    as.Date(date)
    paste0(s, "time(\"", date, "T00:00:00.000Z\")")
  }, error = function(msg) {
    paste0(s, "elevation(", date, ")")
  })
  url = paste0(base_geoserver_(), "format=geotiff&coverageId=", 
                coverage_id, param)
  print(url)
  if (is.null(output_path)) {
    download.file(url, z, method = "libcurl", mode = "wb")
    ras = raster::raster(z)
  }
  else {
    download.file(url, destfile = output_path, method = "libcurl", 
                  mode = "wb")
  }
}

base_geoserver_ = function (env = "ops") 
{
  if (env == "dev") {
    return("https://geoserver-dev.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&")
  }
  else if (env == "ops") {
    return("https://geoserver.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&")
  }
  else {
    return("https://geoserver.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&")
  }
}