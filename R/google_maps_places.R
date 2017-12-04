
api_key <- "AIzaSyC9FKW-kjQlEXjfM3OgMZBJ7xE6zCN1JQI"

#' Search for places on Google Maps
#'
#' Returns up to 60 places and includes street_number, route, neighborhood, locality,
#' administrative_area_level_2, administrative_area_level_1, country, postal_code, lat, lng
#'
#' @param query What you are searching for (e.g. "hotels near Arlington, VA").
#' @param type Optional. Search by Google Maps place type (e.g. "schools").
#' @param location Optional. The latitude/longitude around which to retrieve place
#' information. This must be specified as latitude,longitude.
#' @param radius Optional. Radius in meteres around location to search. Default is
#' 50000 and the max.
#' @param api_key Google places API key.
#'
#' @import jsonlite
#' @export
#' @examples
#' google_maps_place_search(query = "fast food in Fairfaz City VA", api_key = "AIzaSyC9FKW-kjQlEXjfM3OgMZBJ7xE6zCN1JQI")
google_maps_place_search <-
  function(query,
           type = "",
           location = "",
           radius = 50000,
           api_key,
           nptk = ""
  ) {
    url <- utils::URLencode(
      sprintf(
        "https://maps.googleapis.com/maps/api/place/textsearch/json?query=%s&location=%s&radius=%s&type=%s&pagetoken=%s&key=%s",
        query,
        location,
        radius,
        type,
        nptk,
        api_key
      )
    )
    ret <- jsonlite::fromJSON(url)
    res <- ret$results
    tok <- ret$next_page_token
    results <-  list(res)
    Sys.sleep(2)
    # browser()
    while (!is.null(tok)) {
      url <- utils::URLencode(
        sprintf(
          "https://maps.googleapis.com/maps/api/place/textsearch/json?query=%s&location=%s&radius=%s&type=%s&pagetoken=%s&key=%s",
          query,
          location,
          radius,
          type,
          tok,
          api_key
        )
      )
      ret <- jsonlite::fromJSON(url)
      res <- ret$results
      tok <- ret$next_page_token
      results[[length(results) + 1]] <- res
      Sys.sleep(2)
    }
    results
  }


google_maps_place_details_search <- function(place_id, api_key) {
  if (missing(place_id))
    place_id = "ChIJR5lrzim0t4kRqU-HqLCbzQA"

  url <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",
                place_id,
                "&key=",
                api_key)

  detail_search <- jsonlite::fromJSON(url)

  if (!is.null(detail_search) && detail_search != ""){
    address_components <- data.frame(detail_search$result$address_components)

    name <- ""
    if (!is.null(detail_search$result$name))
      name <- detail_search$result$name

    address <- ""
    if (!is.null(detail_search$result$formatted_address))
      address <- detail_search$result$formatted_address

    lng <- ""
    if (!is.null(detail_search$result$geometry$location$lng))
      lng <- detail_search$result$geometry$location$lng

    lat <- ""
    if (!is.null(detail_search$result$geometry$location$lat))
      lat <- detail_search$result$geometry$location$lat

    url <- ""
    if (!is.null(detail_search$result$url))
      url <- detail_search$result$url

    website <- ""
    if (!is.null(detail_search$result$website))
      website <- detail_search$result$website

    # county <- ""
    # if (grepl("level_2", address_components$types))
    #   county <- subset(detail_search$result$address_components, grepl("level_2", address_components$types))$long_name

    types <- ""
    if (!is.null(detail_search$result$types))
      types <- paste(detail_search$result$types, collapse = ",")

    dt_detail <- data.table(name=name,
                            place_id=place_id,
                            address=address,
                            lon=lng,
                            lat=lat,
                            url=url,
                            website=website,
                            # county=county,
                            types=types
    )
    # browser()
  }
}


get_places <-
  function(search_term = "fast food",
           search_area = "Fairfax city",
           location_type = "",
           coord_grid_spdf = readRDS("data/grid51.RDS"),
           key = api_key) {

      library(foreach)
      library(data.table)
    api_search_term <- search_term
    api_area_name <- search_area
    api_type <- location_type
    grid <- coord_grid_spdf

    locations <- data.table::data.table(STATEFP=as.character(grid@data$STATEFP),
                            COUNTYFP=as.character(grid@data$COUNTYFP),
                            NAMELSAD=as.character(grid@data$NAMELSAD),
                            grid@coords)
    my_locations <- locations[NAMELSAD %like% "Fairfax city"]

    places_by_area <- list()
    myCluster <- parallel::makeCluster(8, outfile = "")
    doParallel::registerDoParallel(myCluster)

    places_by_area_list <- foreach::foreach(r = iterators::iter(my_locations, by = "row")) %do% {

      pkey <- key
      type <- api_type
      location <- sprintf("%s,%s", r$Lat, r$Lon)
      radius <- 2000
      query <- api_search_term

      sublist_name <- sprintf("%s_%s_%s",
                              gsub(" ", "_", r$NAMELSAD),
                              substr(r$Lat, 1, 8),
                              substr(r$Lon, 1, 8))

      print(paste(query, type, location, radius, pkey))
      browser()
      response <- google_maps_place_search(query, type, location, radius, pkey)

      l <- list()
      l[[sublist_name]] <- response
      l
    }

    parallel::stopCluster(myCluster)

    for (i in 1:length(places_by_area_list)) {
      places_by_area <- c(places_by_area, places_by_area_list[[i]])
    }

    for (n in names(places_by_area)) {
      for (i in 1:length(places_by_area[[n]])) {
        if (length(places_by_area[[n]][[i]]) > 0) {
          print(paste(n, i))
          assign(
            paste0(n, i),
            data.table(
              lat = unlist(places_by_area[[n]][[i]]$geometry$location$lat),
              lon = unlist(places_by_area[[n]][[i]]$geometry$location$lng),
              name = unlist(places_by_area[[n]][[i]]$name),
              place_id = unlist(places_by_area[[n]][[i]]$place_id),
              address = unlist(places_by_area[[n]][[i]]$formatted_address)
            )
          )
          if (!exists("places_dt_list")) {
            print("creating places_dt_list")
            places_dt_list <- list(get(paste0(n, i)))
          } else {
            print("adding to list")
            places_dt_list[[length(places_dt_list) + 1]] <-
              get(paste0(n, i))
          }
          rm(list = (paste0(n, i)))
        }
      }
    }

    final_dt <- unique(data.table::rbindlist(places_dt_list))
    final_dt
  }


get_place_details <- function(places) {
  final_dt <- data.table::setDT(places)
  dt_place_ids <- final_dt[,.(place_id)]
  dt_place_details <- lapply(dt_place_ids$place_id, place_id2details)
  dt_place_details <- data.table::rbindlist(dt_place_details)
  dt_place_details <- unique(dt_place_details)
  dt_place_details <- dt_place_details[address %like% " VA .*USA$"]
  dt_place_details
}

get_place_fips <- function(place_details) {
  dt_place_details <- data.table::setDT(place_details)
  dt_place_fips <-
    FCClocations2FIPS(
      place_idCol = dt_place_details$place_id,
      lonCol = dt_place_details$lon,
      latCol = dt_place_details$lat
    )
  setkey(dt_place_details, place_id)
  setkey(dt_place_fips, place_id)
  final_places <- dt_place_details[dt_place_fips, nomatch=0]
  final_places
}






# saveRDS(final_places, sprintf("~/git/dashboard/data/GMAPS/%s_by_area_%s_va.RDS", gsub(" ", "_", api_search_term), gsub(" ", "_", api_area_name)))
#
# spdf <- sp::SpatialPointsDataFrame(final_places[,.(lon, lat)], final_places[, !c("lon", "lat")])
# saveRDS(spdf, sprintf("~/git/dashboard/data/GMAPS/%s_by_area_%s_va_spdf.RDS", gsub(" ", "_", api_search_term), gsub(" ", "_", api_area_name)))





