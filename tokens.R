# AppEEARS api for R and grabbing box for NDVI timeseries

## Login
#documentation: <https://lpdaacsvc.cr.usgs.gov/appeears/api/?language=R#login>
#Note: you must enable API access in AppEEARS, for a tutorial see here:https://lpdaacsvc.cr.usgs.gov/appeears/help

library(httr)
library(jsonlite)

username = 'katharyn.duffy'
password = 'P1nyonP1ne'

secret <- base64_enc(paste(username, password, sep = ":"))
response <- POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/login", 
                 add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                             "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                 body = "grant_type=client_credentials")
token_response = response
token_response
rm(username, password)
