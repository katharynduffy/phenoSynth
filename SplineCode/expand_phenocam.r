#' Expand a PhenoCam time series from 3-day to a 1-day time step
#' 
#' Necessary step to guarantee consistent data processing between 1 and 3-day
#' data products. .
#'
#' @param data a PhenoCam dataframe from API
#' @param truncate year (numerical), limit the time series
#' to a particular year (default = NULL)
#' (\code{TRUE} = default/ \code{FALSE} )
#' @return Expanded PhenoCam data structure or file, including 90 day padding
#' if requested.
#' @keywords time series, post-processing, phenocam


expand_phenocam = function(phenocam_data,
                           truncate = NULL,
                           internal = TRUE,
                           out_dir = tempdir()) {
  
  # convert dates
  phenocam_data$dates = as.Date(phenocam_data$date)
  
  ## # remove dates that were filled before expanding again
  ## # this is similar to phenocam_contract() but includes
  ## # the removal of the padding
  
  # truncate the data if necessary
  max_date = max(phenocam_data$date)
  
  # pad with 90 days (regardless)
  min_range = min(phenocam_data$date) - 90
  max_range = max(phenocam_data$date) + 90
  
  
  # create vectors to populate final output with
  all_dates = seq(as.Date(min_range), as.Date(max_range), "days")
  all_years = as.integer(format(all_dates, "%Y"))
  all_doy = as.integer(format(all_dates, "%j"))
  
  # create data frame with dates to merge with original data
  all_dates = as.data.frame(all_dates)
  colnames(all_dates) = "date"
 
  
  output=left_join(all_dates, phenocam_data)
  output$year = all_years
  output$doy = all_doy
  
  # stuff expanded data back into original data structure
  data = output
  
#return data
  
    return(data)
  
}
