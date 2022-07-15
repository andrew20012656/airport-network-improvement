#' dataframe that contains information about proportion of reachable grid points within 75 miles
#' from every point in `lonlat_usa`
#'
#' A dataset with the coordinates of 9285 in lonlat_usa
#'
#' @format a dataframe with 9285 rows and 3 columns
#' \describe{
#'     \item{Var1}{codes for the airport}
#'     \item{Var2}{latitude of airport, in decimal degrees north}
#'     \item{prop.list{proportion of reachable grid points}
#' }
"reachable_grids_within_75_miles"
