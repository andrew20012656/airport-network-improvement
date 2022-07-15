#' test whether a `direct_from` object is symmetric
#'
#' @param direct_from create a list containing information about direct connections from each airport
#' @return return a list whose first element is TRUE or FALSE (whether or not it is symmetric),
#' and whose second element is an `n x 2` character matrix giving the `n` asymmetricconnections.
#'
#' @examples
#' # check if list direct_from is symmetric
#' check_symmetric(direct_from)
#' @export
check_symmetric <- function(df = direct_from){
  # insert code here to check for symmetry
  symm <- TRUE
  # conn <- matrix(NA, nrow = 1, ncol = 2)
  # airports_names <- names(df)
  # for(i in 1:length(df)){
  #   dest <- df[[airports_names[i]]]
  #   if(length(dest) != 0){
  #     for(j in 1:length(dest)){
  #       if(!(airports_names[i] %in% df[[dest[j]]])){
  #         symm <- FALSE
  #         conn <- rbind(conn, c(airports_names[i], dest[j]))
  #       }
  #     }
  #   } else if( length(dest == 0)){
  #
  #   }
  # }
  connection_matrix <- matrix(0, nrow = length(df), ncol = length(df))
  conn <- matrix(NA, nrow = 1, ncol = 2)

  airports_names <- names(df)
  rownames(connection_matrix) <- airports_names
  colnames(connection_matrix) <- airports_names
  for(i in 1:length(df)){
    dests <- df[[airports_names[i]]]
    if(length(dests) > 0){
      for(j in 1:length(dests)){
        connection_matrix[i, dests[j]] <- 1
      }
    }
  }
  symm <- isSymmetric(connection_matrix)
  if(symm == FALSE){
    for(i in 1:nrow(connection_matrix)){
      if(i == 1){
        next
      }
      for(j in 1:(i-1)){
        if(isTRUE(connection_matrix[i,j] != connection_matrix[j,i])){
          conn <- rbind(conn, c(airports_names[i], airports_names[j]))
        }
      }
    }
    return(list(symm = symm, conn = conn[2:nrow(conn),]))
  } else if (symm==TRUE) {
    return( list(symm = symm, conn = conn))
  }
}
