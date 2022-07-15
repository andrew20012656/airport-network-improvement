
#' Compute the points reachable from a lonlat coordinate by traveling less than 2x miles
#'
#' Given a pair of longtiude/latitude coordinate and return the grids reachable from
#' this coordinate.
#'
#' @param data a n1 x 2 dataframe, first column longitude, second column latitudes
#' @param lonlat an n2 x 2 matrix, first column longitudes, second column latitudes
#' @param x a positive number
#' @param df a list containing the direct_from information
#' @return return an n3 x 2 dataframe containing all the grids reachable from the given
#' coordinate
#'
#' @examples
#
#' @export
reachable_points <- function(data,lonlat,x,df = direct_from, ad = airport_data){
  results <- rep(FALSE, nrow(data))
  dists <- sp::spDistsN1(data, lonlat, longlat = TRUE)
  # part (a)
  # Check if within crow distance
  # Convert dists from km to miles
  dists <- dists / 1.60934
  in_crow_dist <- which(dists <= 2*x)
  results[in_crow_dist] <- TRUE
  # part (b)
  airports <- airports_nearby(lonlat, ad, x)
  print("airports_nearby ok")
  # check if there's airports near by
  if (nrow(airports)== 0) {
    reachable_df <- data.frame(data[results,])
    print("2 ok")
    return(reachable_df) # no airports nearby and just return condition a
  } else {
    airports_in_one_flight <- c()
    for(i in 1:nrow(airports)){
      airport_code <- airports[i,1]
      dests <- df[[airport_code]]
      if(length(dests) == 0){
        next
      }
      airports_in_one_flight <- append(airports_in_one_flight, dests)
    }
    print("pull out all reachable airports in one flight ok")
    airports_in_one_flight <- unique(airports_in_one_flight)
    if(length(airports_in_one_flight) == 0){
      reachable_df <- data.frame(data[results,])
      return(reachable_df)
    }
    ports_lonlat <- ad[(ad$airport_code) %in% airports_in_one_flight, 2:3]
    print("3 ok")
    for(i in 1:nrow(ports_lonlat)){
      port_dists <- sp::spDistsN1(data, c(ports_lonlat[i,1], ports_lonlat[i,2]), longlat = TRUE)
      port_dists <- port_dists / 1.60934
      in_dist <- which(port_dists <= x)
      results[in_dist] <- TRUE
    }
    print("reachable grids from dest airport ok")
    reachable_df <- data.frame(data[results,])
    print("4 ok")
    return(reachable_df)
  }
}
