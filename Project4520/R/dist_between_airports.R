#' A matrix containing the distance between every pair of airports
#'
#' This matrix is built from airline_2019-07-01.csv which contains all the flights information.
#' The row names are the names of the airports where flights depart. The column names are the
#' names of the destination airports. The matrix is not symmetric, which means that A flies to
#' B doesn't imply that B also flies to A.
#' 
#' @format a matrix of 354 x 354
"dist_between_airports"