library(jsonlite)
library(data.table)

#' Convert FCC location to FIPS code
#'
#' @param place_id some unique identifier for the lat lon
#' @param lat the latitude
#' @param lon the longitude
#' @return data.table
#' @export
#' @examples
#' sdalr::FCClocation2FIPS("VTRC", lat=38.880807, lon=-77.11577)
FCClocation2FIPS <- function(place_id, lat, lon) {
    json_string <- sprintf('http://data.fcc.gov/api/block/find?format=json&latitude=%s&longitude=%s&showall=true&format=JSON',
                           lat, lon)
    if (length(place_id) > 1) {stop('you supplied multiple values for place_id, did you mean to use FCClocations2FIPS?')}
    if (length(lat) > 1) {stop('you supplied multiple values for lat, did you mean to use FCClocations2FIPS?')}
    if (length(lon) > 1) {stop('you supplied multiple values for lon, did you mean to use FCClocations2FIPS?')}

    res <- jsonlite::fromJSON(json_string)
    data.table::data.table(place_id = place_id,
                           state_name = res$State$name,
                           state_fips = res$State$FIPS,
                           county_name = res$County$name,
                           county_fips = res$County$FIPS,
                           block_fips = res$Block$FIPS)
}

#' Convert multiple FCC locations to FIPS codes
#'
#' @param place_idCol vector of unique identifiers
#' @param latCol vector of latitudes
#' @param lonCol vector of longitudes
#' @return data.table
#' @export
#' @examples
#' sdalr::FCClocations2FIPS(place_idCol = c("VTRC", "VT-NVC"),
#'                          latCol = c(38.880807, 38.8968325),
#'                          lonCol = c(-77.11577, -77.1894815))
FCClocations2FIPS <- function(place_idCol, latCol, lonCol) {
    res <- data.table::as.data.table(t(mapply(sdalr::FCClocation2FIPS, place_idCol, latCol, lonCol)))
    data.table::data.table(place_id = unlist(res$place_id),
                           state_name = unlist(res$state_name),
                           state_fips = unlist(res$state_fips),
                           county_name = unlist(res$county_name),
                           county_fips = unlist(res$county_fips),
                           block_fips = unlist(res$block_fips))
}
