library(httr)
library(jsonlite)


CuratedApiClient <- setClass(
    "CuratedApiClient",
    slots = c(
        baseUrl = "character",
        user = "character",
        password = "character",
        token = "character",
        tokenExpiration = "character",
        tokenExpirationFormat = "character",
        sampleServices = "vector"
    ),
    prototype = list(
        baseUrl = "https://lpdaacsvc.cr.usgs.gov/services/curated-api",
        token = "",
        tokenExpiration = "",
        tokenExpirationFormat = "%Y-%m-%dT%H:%M:%OS",
        sampleServices = c('scidb', 'file', 'opendap')
    )
)

setGeneric("sampleServices", function(this) standardGeneric("sampleServices"))
setMethod("sampleServices", "CuratedApiClient", function(this) {
    return(this@sampleServices)
})

setGeneric("baseUrl", function(this) standardGeneric("baseUrl"))
setMethod("baseUrl", "CuratedApiClient", function(this) {
    return(this@baseUrl)
})

setGeneric("tokenIsExpired", function(this) standardGeneric("tokenIsExpired"))
setMethod("tokenIsExpired", "CuratedApiClient", function(this) {
    if (this@token == "" | this@tokenExpiration == "") { # switched or to |
        stop("No token exists to check for expiration")
    }

    now <- Sys.time()
    expiration <- strptime(this@tokenExpiration, this@tokenExpirationFormat) # removed @ before this@tokenExpiration
    return(if(now > expiration) TRUE else FALSE)
})

setGeneric("getCatalog", function(this) standardGeneric("getCatalog"))
setMethod("getCatalog", "CuratedApiClient", function(this) {
    url = sprintf("%s/catalog", this@baseUrl)
    response <- GET(url)
    json <- prettify(toJSON(content(response)))

    return(json)
})
