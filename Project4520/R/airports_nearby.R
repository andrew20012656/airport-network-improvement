#' find all airports in `airport_data` within `x` miles of a longitude latitude point
#'
#' given a longitude/latitude coordinate and return the information about airports(airport_code, longitude,
#' latitude) that are within x miles
#'
#' @param lonlat a length-2 vector or a 1 x 2 matrix containing a longitude and latitude
#' @param airport_data the airport_data frame object
#' @param x within x miles of lonlat, default x=75 miles
#' @return return a data frame that includes the information about airports(airport_code, longitude,
#' latitude) that are within x miles
#'
#' @examples
#' # airports within 75 miles of Ithaca
#' airports_nearby(matrix(c(-76.45872,42.49136),1,2,byrow=TRUE),airport_data)
#' # airports within 75 miles of Ithaca, lonlat as length 2 vector
#' airports_nearby(c(-76.45872,42.49136),airport_data,75)
#' # airports within 100 miles of Ithaca
#' airports_nearby(matrix(c(-76.45872,42.49136),1,2,byrow=TRUE),airport_data,100)
#' @export
airports_nearby <- function(lonlat, airport_data, x = 75){
  lonlat <- matrix(c(lonlat[1],lonlat[2]),nrow=1,ncol =2)
  dist <- distfun(lonlat, airport_data[, 2:3])
  within_miles <- airport_data[which(dist<x),]
  return (within_miles)
}

# airports_nearby <- function(lonlat, airport_data, x = 75){
#  # ports <- data.frame(airport_code = c("temp"), longitude.degree = c(0), #latitude.degree = c(0))
#   ports <- data.frame(airport_code = NA, longitude.degree = NA, latitude.degree = NA)
#   for(i in 1:nrow(airport_data)){
#     dist <- distfun(lonlat, airport_data[i, 2:3])[1,1]
#     if(dist <= x){
#       ports <- rbind(ports, airport_data[i,])
#     }
#   }
#
#   if (nrow(ports) == 1) {
#     return(ports)
#   } else {
#     ports <- ports[2:nrow(ports),]
#     return(ports)
#   }
#  # ports <- ports[2:nrow(ports),]
#   #return(ports)
# }
