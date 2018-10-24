
# is not null function
is_not_null = function(x) ! is.null(x)


# not in
'%!in%'     = function(x, y) {
  ! ('%in%'(x, y))
}

# Returns Downloads folder for windows/macos
get_download_folder = function(){
  if (Sys.info()['sysname'] == 'Darwin'){
    folder = paste('/Users/', Sys.getenv('LOGNAME'),'/Downloads/', sep = '')
  }else if (Sys.info()['sysname'] == 'Windows'){
    folder = paste('C:/Downloads/', sep = '')
  }else{
    folder = ''
  }
  return (folder)
}


# Deletes the netcdf from input filepath
delete_file = function(filepath_){
  if (file.exists(filepath_)) file.remove(filepath_)
}

