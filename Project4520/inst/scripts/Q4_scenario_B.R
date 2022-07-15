library("Project4520")
airports <- continent_df$airport_code

proportion.val <- function(reachable_df) {
  prop <- nrow(reachable_df) / nrow(lonlat_usa)
  return (prop)
}

prop.reacheable.points <- function(lonlat_usa,x, df = direct_from, ad = airport_data) {
  prop.list<- rep(NA, length.out=(nrow(lonlat_usa)))
  for (i in 1:nrow(lonlat_usa)) {
    print(sprintf("Iterations progress: %0.5f", i/nrow(lonlat_usa)))
    print(sprintf("current_iteration: %.2f", i))
    grid_point <- matrix(c(lonlat_usa[i,][[1]],lonlat_usa[i,][[2]]), nrow = 1, ncol = 2)
    reachable <- reachable_points(lonlat_usa,grid_point,x, df, ad)
    prop.list[i] <- proportion.val(reachable)
  }
  return (cbind(lonlat_usa,prop.list))
}

### connection_mat: 1/0 to represent if there is connection between U.S. airports
connection_mat <- connection_matrix[airports, airports]

## direct_from of all airports in the continetal USA
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

head(continent_df)
dim(as.matrix(continent_df[which(continent_df$airport_code == "ITH"),2:3]))


change_connections <- function(target_connections, x, n){
  tracker_b <- data.frame(A = c("A"), B = c("B"), C = c("C"), D = c("D"))
  new_df <- df_in_USA
  for(i in 1:n){
    indexes <- sample.int(nrow(target_connections), size = 2)
    A <- target_connections[indexes[1], 1]
    B <- target_connections[indexes[1], 2]
    C <- target_connections[indexes[2], 1]
    D <- target_connections[indexes[2], 2]
    A_lonlat <- continent_df[which(continent_df$airport_code == A),2:3]
    B_lonlat <- continent_df[which(continent_df$airport_code == B),2:3]
    C_lonlat <- continent_df[which(continent_df$airport_code == C),2:3]
    D_lonlat <- continent_df[which(continent_df$airport_code == D),2:3]
    AC_dist <- distfun(as.matrix(A_lonlat), as.matrix(C_lonlat))[1,1]
    BD_dist <- distfun(as.matrix(B_lonlat), as.matrix(D_lonlat))[1,1]
    if(length(unique(c(A,B,C,D))) != 4){
      next
    }
    if((A %in% new_df[[C]]) | (B %in% new_df[[D]])){
      next
    }
    if((AC_dist >= x & AC_dist != 0) & (BD_dist >= x & BD_dist != 0)){
      new_df[[A]] <- new_df[[A]][-which(new_df[[A]] == B)]
      new_df[[B]] <- new_df[[B]][-which(new_df[[B]] == A)]
      new_df[[C]] <- new_df[[C]][-which(new_df[[C]] == D)]
      new_df[[D]] <- new_df[[D]][-which(new_df[[D]] == C)]

      new_df[[A]] <- append(new_df[[A]], C)
      new_df[[B]] <- append(new_df[[B]], D)
      new_df[[C]] <- append(new_df[[C]], A)
      new_df[[D]] <- append(new_df[[D]], B)

      target_connections <- target_connections[-indexes,]
      tracker_b <- rbind(tracker_b, c(A, B, C, D))
    } else{
      next
    }
  }
  return(list(new_df, tracker_b[-1,]))
}

dat
# See how many existing flights are within 150 miles.
# The reason checking 150 miles because crow distance = 150 miles when x = 75 mi
# When x = 100 mi, the crow distance = 200 mi.
# In this x = 100 case, the existing flights within 150 miles will also be included
# Thus, we want to improve the average proportion of reachable grids by modifying
# these existing flights

dat <- check_connections(150)
# Get the new direct_from object by removing the connections of flights within
# 200 miles and add new flights with distance greater than 200 miles.
df_b_100 <- change_connections(dat, 200, 100)

direct_from_b <- df_b_100[[1]]
scenario_b_change <- df_b_100[[2]]
scenario_b_prop_100 <- prop.reacheable.points(lonlat_usa, 100, df = direct_from_b, ad = continent_df[,1:3])
scenario_b_prop_75 <- prop.reacheable.points(lonlat_usa, 75, df = direct_from_b, ad = continent_df[,1:3])

save(direct_from_b,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/direct_from_b.RData")
save(scenario_b_change,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/scenario_b_change.RData")
save(scenario_b_prop_100,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/scenario_b_prop_100.RData")
save(scenario_b_change_75,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/scenario_b_prop_75.RData")

mean(scenario_b_prop_100[,3])
mean(prop_reachable_100[,3])

mean(scenario_b_prop_75[,3])
mean(prop_reachable_75[,3])

