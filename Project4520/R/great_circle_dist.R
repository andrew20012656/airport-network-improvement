
#' compute the great circle distance between pairs of longitude/latitude coordinates
#'
#' given a pair of longtiude/latitude coordinate and return the great circle distance
#' between two points
#'
#' @param lonlat1 an n1 x 2 matrix, first column longitudes, second column latitudes
#' @param lonlat2 an n2 x 2 matrix, first column longitudes, second column latitudes
#' @return return an n1 x n2 matrix of distances. distmat[i,j] is the distance between
#' lonlat1[i,] and lonlat2[j,]
#'
#' @examples
#' # lonlat1 contains 2 x 2 matrix:
#' lonlat1 <- matrix(c(-76.10631,43.11119,-87.90815,41.97694),2,2,byrow=TRUE)
#' # lonlat2 contains 1 x 2 matrix
#' lonlat2 <- matrix(c(-76.45872,42.49136),1,2,byrow=TRUE)
#' distfun(lonlat1,lonlat2)
#' # returns dismat with 2 x 1 matrix
#' @export
distfun <- function(lonlat1,lonlat2 ){
  distmat <- matrix(NA,nrow(lonlat1),nrow(lonlat2))
  for (i in 1:nrow(lonlat1)) {
    for (j in 1:nrow(lonlat2)) {
      distmat[i,j] <- geosphere::distCosine(as.vector(lonlat1[i,]), as.vector(lonlat2[j,]), r = 3958.8)
    }
  }
  return(distmat)
}

