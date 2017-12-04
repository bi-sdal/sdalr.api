#' Geocode an address.
#'
#' Returns street_number, route, neighborhood, locality, administrative_area_level_2,
#' administrative_area_level_1, country, postal_code, lat, lng
#'
#' @param address_str Street address string.
#' @param api_key Optional. Google geocoding API key. Needed if getting more than 2500 results in 1 day.
#' @import jsonlite
#' @export
#' @examples
#' google_geocode("900 N Glebe Rd, Arlington, VA 22203", "AIzaSyC9FKW-kjQlEXjfM3OgMZBJ7xE6zCN1JQI")
google_maps_geocode <- function(address_str = "900 N Glebe Rd, Arlington, VA 22203",
  api_key = "") {
  url <- utils::URLencode(sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s",
    address_str, api_key))

  response <- jsonlite::fromJSON(url)

  if (response$status[[1]] == "OK") {
    vals <- as.data.frame(t(response$results$address_components[[1]]$short_name))
    num_col <- length(response$results$address_components[[1]]$types)
    cols <- list()
    for (i in 1:num_col) {
      cols <- c(cols, response$results$address_components[[1]]$types[[i]][[1]])
    }
    colnames(vals) <- cols
    vals$lat <- if (length(response$results$geometry$location$lat) ==
      1)
      response$results$geometry$location$lat else NA
    vals$lng <- if (length(response$results$geometry$location$lng) ==
      1)
      response$results$geometry$location$lng else NA
    vals
  } else {
    response$status
  }
}
