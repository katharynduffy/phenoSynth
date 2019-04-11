
# Converts the highlighted pixel coords into a spatialpolygon class
matrix_to_polygon = function(matrix, id, type_, crs = '+proj=longlat +datum=WGS84'){
  p   = Polygon(matrix)
  ps  = Polygons(list(p), ID = id)
  sps = SpatialPolygons(list(ps))
  proj4string(sps) = CRS(crs)
  return (sps)
}


# Builds a color palet for the modis landcover raster layer
build_pft_palette = function(raster_){
  print ('building palet')
  colors = c()
  names  = c()
  color_list    = c('#1b8a28', '#36d03e', '#9ecb30', '#a0f79f', '#91bb88', '#b99091', '#f0dfb8', '#d6ed9a',
                    '#f1dc07', '#ecbb5b', '#4981b1', '#fcee72', '#fd0608', '#9b9353', '#bdbec0', '#bdbec0', '#89cae3')
  v = unique(values(raster_))
  remove = c(NA)
  v = v [! v %in% remove]
  v = sort(v, decreasing = FALSE)
  
  for (x in v){
    if (x == 17){
      colors = c(colors,color_list[17])
      name   = as.character(subset(pft_df, pft_df$pft_key == 0)$pft_expanded)
      names  = c(names, name)
    }else{
      colors = c(colors, color_list[x])
      name   = as.character(subset(pft_df, pft_df$pft_key == x)$pft_expanded)
      names  = c(names, name)
    }
  }
  colors_ = list('colors' = colors, 'names' = names)
  return (colors_)
}


# Build grid for any input raster
build_raster_grid = function(raster_, map_ = NULL){
  r_         = raster_
  xmin       = xmin(extent(r_))
  xmax       = xmax(extent(r_))
  ymin       = ymin(extent(r_))
  ymax       = ymax(extent(r_))
  nrows      = nrow(r_)
  ncols      = ncol(r_)
  resolution = res(r_)[1]
  
  lats = c()
  lons = c()
  ids  = c()
  for (x in c(0:ncols)){
    id = x
    lat1 = ymax
    lat2 = ymin
    
    lon1 = xmin + (x * resolution)
    lon2 = xmin + (x * resolution)
    
    lats = c(lats, lat1, lat2)
    lons = c(lons, lon1, lon2)
    ids  = c(ids, id, id)
  }
  
  for (xx in c(0:nrows)){
    id = xx + x
    lat1 = ymax - (xx * resolution)
    lat2 = ymax - (xx * resolution)
    
    lon1 = xmax
    lon2 = xmin
    
    lats = c(lats, lat1, lat2)
    lons = c(lons, lon1, lon2)
    ids  = c(ids, id, id)
  }
  
  df.sp = data.frame(id=ids, latitude=lats, longitude=lons)
  if (class(df.sp) == 'data.frame'){
    coordinates( df.sp ) = c( "longitude", "latitude" )
    id.list = sp::split( df.sp, df.sp[["id"]] )
    id = 1
    # For each id, create a line that connects all points with that id
    for ( i in id.list ) {
      event_lines = SpatialLines(list(Lines(Line(i[1]@coords), ID = id)),
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
      if (id == 1){
        sp_lines  = event_lines
      } else {
        sp_lines  = rbind(sp_lines, event_lines)
      }
      id = id + 1
    }
    sp_lines
  }else{
    print ('already a sp object')
  }
  is_not_null = function(x) ! is.null(x)
  if (is_not_null(map_)){
    print ('Adding Raster grid to map')
    leafletProxy(map_) %>% addPolylines(data = sp_lines, weight = 1.8, opacity = 1, color = 'grey', group = '250m MODIS Grid') %>%
      addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                       overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '500m Highlighted Pixels', '250m Highlighted Pixels', '250m MODIS Grid'),
                       position = c("topleft"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup('500m Highlighted Pixels') %>%
      showGroup('500m Highlighted Pixels') %>%
      hideGroup('250m Highlighted Pixels') %>%
      showGroup('250m Highlighted Pixels')
  }else{
      return (sp_lines)
    }
}


# Creates boundary box for clipping rasters using lat/lon from phenocam site
crop_raster = function(lat_, lon_, r_, reclassify=FALSE, primary=NULL){
  height = .03
  width  = .05
  e      = as(extent(lon_-width, lon_ + width, lat_ - height, lat_ + height), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r        = raster::crop(r_, e, snap='near')
  
  if (reclassify == FALSE){
    return (r)
    
  }else if (reclassify == TRUE){
    
    water = 17*2
    
    m = c(1,2,
          2,2,
          3,2,
          4,2,
          5,2,
          6,2,
          7,2,
          8,2,
          9,2,
          10,2,
          11,2,
          12,2,
          13,2,
          14,2,
          15,2,
          16,2,
          17,2)
    
    if(!is.null(primary)){
      prim    = primary*2
      m[prim] = 1
    }
    
    rclmat = matrix(m, ncol=2, byrow=TRUE)
    rc     = raster::reclassify(r, rclmat)
    if (length(unique(values(rc))) == 1){
      
      m = c(1,NA,
            2,NA,
            3,NA,
            4,NA,
            5,NA,
            6,NA,
            7,NA,
            8,NA,
            9,NA,
            10,NA,
            11,NA,
            12,NA,
            13,NA,
            14,NA,
            15,NA,
            16,NA,
            17,NA)
      
      rclmat = matrix(m, ncol=2, byrow=TRUE)
      rc     = raster::reclassify(r, rclmat)
    }
    return (rc)
  }
}



# Creates a reprojection of a lat/lon WGS84 point into sinusoidal Modis projection
get_x_y_sinu_from_wgs_pt = function(lon_,lat_){
  xy              = data.frame(matrix(c(lon_,lat_), ncol=2))
  colnames(xy)    = c('lon', 'lat')
  coordinates(xy) = ~ lon + lat
  proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
  p               = spTransform(xy, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
  return (p)
}


# Creates a reprojection of a lat/lon WGS84 point into sinusoidal Modis projection
get_lat_lon_wgs_from_sinu_pt = function(lon_,lat_){
  print ('Reprojecting coords to WGS84')
  xy              = data.frame(matrix(c(lon_,lat_), ncol=2))
  colnames(xy)    = c('lon', 'lat')
  coordinates(xy) = ~ lon + lat
  proj4string(xy) = CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  p               = spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  print (coordinates(xy))
  print (coordinates(p))
  return (p)
}


# Radians to degrees
rad_to_deg = function(rad) {
  (rad * 180) / (pi)
}


# Given row from sites, create points for polyline from site.
#   This function uses angle for field of view and los as the
#   far distance of the FOV.
run_add_polyline = function(site_data_, azm_){
  los = .01
  lat =  site_data_$Lat
  lon =  site_data_$Lon
  dst = sqrt(los**2 + los**2)
  c   = rotate_pt(lon, lat, (azm_-25), dst)
  b   = rotate_pt(lon, lat, (azm_+25), dst)
  cx  = c[[1]]
  cy  = c[[2]]
  bx  = b[[1]]
  by  = b[[2]]
  
  datalon = c(lon,cx,bx,lon)
  datalat = c(lat,cy,by,lat)
  camera  = site_data_$Sitename
  id_     = paste('fov',camera, sep='')
  add_polyline(datalon, datalat, id_ = 'azm_', .45, 'red', group = 'azm_')
}


# Rotate a point based on AZM
rotate_pt = function(lon, lat, azm, r){
  rad  = azm * (pi / 180)
  lon_ = lon + (r * sin(rad))
  lat_ = lat + (r * cos(rad))
  return (list(lon_, lat_))
}
