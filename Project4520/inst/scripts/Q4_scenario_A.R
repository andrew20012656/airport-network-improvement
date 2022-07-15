library(Project4520)
load("./../../data/reachable_grids_within_75_miles.RData")
load("./../../data/lonlat_usa.RData")
library("Project4520")
airports <- continent_df$airport_code

proportion.val <- function(reachable_df) {
  prop <- nrow(reachable_df) / nrow(lonlat_usa)
  return (prop)
}

prop.reacheable.points <- function(lonlat_usa,x, df = direct_from, ad = airport_data) {
  prop.list<- rep(NA, length.out=(nrow(lonlat_usa)))
  for (i in 1:nrow(lonlat_usa)) {
    print(sprintf("Iterations progress: %f0.5f", i/nrow(lonlat_usa)))
    print(sprintf("current_iteration: %.2f", i))
    grid_point <- matrix(c(lonlat_usa[i,][[1]],lonlat_usa[i,][[2]]), nrow = 1, ncol = 2)
    reachable <- reachable_points(lonlat_usa,grid_point,x, df, ad)
    prop.list[i] <- proportion.val(reachable)
  }
  return (cbind(lonlat_usa,prop.list))
}

### connection_mat: 1/0 to represent if there is connection between U.S. airports
connection_mat <- connection_matrix[airports, airports]

## direct_from of all airports in the continental USA
val <- rownames(connection_mat) # get the row names for the origin
df_in_USA <- vector(mode = "list", length = nrow(connection_mat )) # create an empty list of all origin
for (i in 1:length(rownames(connection_mat ))) {
  airport_val <- rep()
  names(df_in_USA)[i] <- val[i]
  for (j in 1:length(rownames(connection_mat))) {
    if (isTRUE(connection_mat[val[i],val[j]] != 0) | isTRUE(connection_mat[val[j],val[i]] != 0)){
      airport_val <- append(airport_val,val[j])
      df_in_USA[[val[i]]] <- airport_val
    }
  }
}

close_airports_share_dests <- function(x){
  n <- nrow(dist_between_airports)
  close_airports <- data.frame()
  for(i in 1:n){
    for(j in 1:i){
      if(dist_between_airports[i,j] <= x & dist_between_airports [i,j] != 0){
        A <- rownames(dist_between_airports)[i]
        B <- rownames(dist_between_airports)[j]
        close_airports <- rbind(close_airports, c(A,B))
      }
    }
  }
  colnames(close_airports) <- c("A", "B")
  print(head(close_airports))
  result <- data.frame()
  for(k in 1:nrow(close_airports)){
    dests_A <- df_in_USA[[close_airports[k,"A"]]]
    dests_B <- df_in_USA[[close_airports[k,"B"]]]
    if(length(dests_A) < length(dests_B)){
      shared_dests <- dests_A[dests_A %in% dests_B]
      if(length(shared_dests) > 0){
        result <- rbind(result, c(close_airports[k,"A"], close_airports[k,"B"]))
      }
    } else{
      shared_dests <- dests_B[dests_B %in% dests_A]
      if(length(shared_dests)){
        result <- rbind(result, c(close_airports[k,"A"], close_airports[k,"B"]))
      }
    }
  }
  names(result) <- c("A", "B")
  return(result)
}

temp <- close_airports_share_dests(150)

dat
dist_matrix_usa <- dist_between_airports[airports, airports]

## check the flight distance that is within x miles
check_connections <- function(x) {
  target_connnection <- data.frame()
  airport_names <- rownames(dist_matrix_usa)
  count <- 0
  for(i in 1:nrow(dist_matrix_usa)){
    for (j in 1:i) {
      count <- count + 1
      if (dist_matrix_usa[i,j] <= x & dist_matrix_usa[i,j] !=0){
        airport.A <- airport_names[i]
        airport.B <- airport_names[j]
        target_connnection <- rbind(target_connnection,c(airport.A,airport.B))
      }
    }
  }
  names(target_connnection) <- c("origin","dest")
  return (target_connnection)
}

change_connections <- function(target_connections, x){
  n <- nrow(target_connections)
  tracker_a <- data.frame(A = c("A"), B = c("B"), C = c("C"))
  new_df <- df_in_USA
  for(i in 1:n){
    indexes <- sample.int(nrow(target_connections), size =1)
    ## remove a connection that has flight distance within x
    A <- target_connections[indexes, 1]
    B <- target_connections[indexes, 2]
    print(sprintf("A: %s", A))
    print(sprintf("B %s", B))

    # Choose an airline to be added
    index_existing_airlines <- which(dist_matrix_usa[A,] == 0)
    index_dest <- index_existing_airlines[sample.int(length(index_existing_airlines), size = 1)]
    C <- colnames(dist_matrix_usa)[index_dest]

    # check the if the candidate distance connection is greater than x miles
    A_lonlat <- continent_df[which(continent_df$airport_code == A),2:3]
    C_lonlat <- continent_df[which(continent_df$airport_code == C),2:3]
    AC_dist <- distfun(as.matrix(A_lonlat), as.matrix(C_lonlat))[1,1]
    print(AC_dist)

    if(length(unique(c(A,B,C))) != 3){
      next
    }
    if(AC_dist > x){
      new_df[[A]] <- new_df[[A]][-which(new_df[[A]] == B)]
      new_df[[B]] <- new_df[[B]][-which(new_df[[B]] == A)]
      new_df[[A]] <- append(new_df[[A]], C)
      new_df[[C]] <- append(new_df[[C]], A)

      target_connections <- target_connections[-indexes,]
      tracker_a <- rbind(tracker_a, c(A, B, C))
    } else{
      next
    }
  }
  return(list(new_df, tracker_a[-1,]))
}
dist_matrix_usa["ITH",]
dat <- check_connections(150)
df_a_100 <- change_connections(dat, 200)

direct_from_a <- df_a_100[[1]]
scenario_a_change <- df_a_100[[2]]
scenario_a_prop_100 <- prop.reacheable.points(lonlat_usa, 100, df = direct_from_a, ad = continent_df[,1:3])
scenario_b_prop_75 <- prop.reacheable.points(lonlat_usa, 75, df = direct_from_a, ad = continent_df[,1:3])

save(scenario_a_change,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/scenario_a_change.RData")
save(direct_from_a,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/direct_from_a.RData")
save(scenario_a_prop_100,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/scenario_a_prop_100.RData")
scenario_a_prop_75 <- scenario_b_prop_75
save(scenario_a_prop_75,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/scenario_a_prop_75.RData")

aa <- c(1,2,3,4)
bb <- c(1,2,3)
aa %in% bb
bb %in% aa
