library(Project4520)
load("./../../data/reachable_grids_within_75_miles.RData")
load("./../../data/lonlat_usa.RData")

# --------------------------------------
# we use the airports within lonlat_usa
airports <- continent_df$airport_code

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

#save(df_in_USA, file = "/Users/shirley/Desktop/statcomp2022/BTRY_4520/Project4520/data/df_in_USA.RData")

# Calculate the proportion of Continental USA grids that can be reached
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

### Scenario A:
n = 2
base_proportion = mean(prop.x.75[,3])
tracker <- data.frame(list(original_depart = c("Empty"),
                           original_dest_1 = c("Empty"),
                           original_dest_1 = c("Empty"),
                           new_depart = c("Empty"),
                           new_dest_1 = c("Empty"),
                           new_dest_2 = c("Empty"),
                           avg_prop = c(0),
                           improved = c(0)))

df <- df_in_USA
cm <- connection_mat
new_airport_data <- continent_df[,1:3]
current_prop <- base_proportion

for(i in 1:n){
  df1 <- df
  cm1 <- cm
  # ------------------------------------------------
  # Choose two airlines to be removed in one airport
  index_depart <- sample.int(nrow(cm1), size =1)
  depart_name <- rownames(cm1)[index_depart]
  index_existing_airlines <- which(cm1[index_depart,] == 1)
  index_dest <- index_existing_airlines[sample.int(length(index_existing_airlines), size = 2)]
  dest_name <- colnames(cm1)[index_dest]
  # ---------------------------------------------
  # Choose two airlines to be added in one airport
  index_depart2 <- sample.int(nrow(cm1), size =1)
  depart_name2 <- rownames(cm1)[index_depart2]
  index_existing_airlines2 <- which(cm1[index_depart2,] == 0)
  index_dest2 <- index_existing_airlines2[sample.int(length(index_existing_airlines2), size = 2)]
  dest_name2 <- colnames(cm1)[index_dest2]
  # Record the chosen airports
  # -------------------------------------------------
  tracker[i, "original_depart"] <- depart_name
  tracker[i, "original_dest_1"] <- dest_name[1]
  tracker[i, "original_dest_2"] <- dest_name[2]
  tracker[i, "new_depart"] <- depart_name2
  tracker[i, "new_dest_1"] <- dest_name2[1]
  tracker[i, "new_dest_2"] <- dest_name2[2]
  # Remove the chosen airline
  # -------------------------------------------------
  print(sprintf("The 1st airline removed is %s - %s", depart_name, dest_name[1]))
  print(sprintf("The 2nd airline removed is %s - %s", depart_name, dest_name[2]))
  for (j in 1:2) {
    cm1[index_depart, index_dest[j]] <- 0
    cm1[index_dest[j], index_depart] <- 0
    df1[[depart_name]] <- df1[[depart_name]][-which(df1[[depart_name]] == dest_name[j])]
    df1[[dest_name[j]]] <- df1[[dest_name[j]]][-which(df1[[dest_name[j]]] == depart_name)]
  }
  # -------------------------------------------------
  # Add the chosen airline
  print(sprintf("The 1st airline added is %s - %s", depart_name2, dest_name2[1]))
  print(sprintf("The 2nd airline removed is %s - %s", depart_name2, dest_name2[2]))
  for (j in 1:2) {
    cm1[index_depart2, index_dest2[j]] <- 1
    cm1[index_dest2[j], index_depart2] <- 1
    df1[[depart_name2]] <- append(df1[[depart_name2]], dest_name2[j])
    df1[[dest_name2[j]]] <- append(df1[[dest_name2[j]]], depart_name2)
  }
  # -------------------------------
  # Evaluate if the new airline increases the reachable grids
  print(sprintf("Check symmetric: %s", check_symmetric(df1)))
  new_prop <- mean(prop.reacheable.points(lonlat_usa, 75, df = df1, ad = new_airport_data)[,3])
  print(sprintf("The old average proportion is %.5f", current_prop))
  print(sprintf("The new average proportion is %.5f", new_prop))
  tracker[i, "avg_prop"] <- new_prop
  if( new_prop > current_prop){
    tracker[i, "improved"] <- 1
    print("The change improves the average proportion of reachable grids!")
    df <- df1
    cm <- cm1
    currect_prop <- new_prop
  } else{
    print("The change didn't improve the average proportion of reachable grids.")
    tracker[i, "improved"] <- 0
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
  result <- data.frame()
  for(k in 1:nrow(close_airports)){
    dests_A <- df_in_USA[[close_airports[k,"A"]]]
    dests_B <- df_in_USA[[close_airports[k,"B"]]]
    if(length(dests_A) < length(dests_B)){
      shared_dests <- dests_A[dests_A %in% dests_B]
      if(length(shared_dests) > 0){
        for(l in shared_dests){
          result <- rbind(result, c(close_airports[k,"A"], close_airports[k,"B"], l))
        }
      }
    } else{
      shared_dests <- dests_B[dests_B %in% dests_A]
      if(length(shared_dests) > 0){
        for(l in shared_dests){
          result <- rbind(result, c(close_airports[k,"A"], close_airports[k,"B"], l))
        }
      }
    }
  }
  names(result) <- c("A", "B", "C")
  return(result)
}

# airports within 150 miles from each other that have the same dest
targets <- close_airports_share_dests(150)
head(targets)
save(targets,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/inst/scripts/targets.RData")

new_df <- df_in_USA
tracker <- data.frame()
for(i in 1:length(unique(targets$A))){
  A1 <- unique(targets$A)[i]
  sets <- targets[which(targets$A == A1),]
  print(sets)
  new_set <- tapply(sets, sets$C, unique)
  if(length(new_set)>= 2){
    a1 <- new_set[[1]][1,1]
    b1 <- new_set[[1]][1,2]
    shared1 <- new_set[[1]][1,3]
    a2 <- new_set[[2]][1,1]
    b2 <- new_set[[2]][1,2]
    shared2 <- new_set[[2]][1,3]
    new_df[[a1]] <- new_df[[a1]][-which(new_df[[a1]] == shared1)]
    new_df[[shared1]] <- new_df[[shared1]][-which(new_df[[shared1]] == a1)]
    new_df[[a2]] <- new_df[[a2]][-which(new_df[[a2]] == shared2)]
    new_df[[shared2]] <- new_df[[shared2]][-which(new_df[[shared2]] == a2)]
    dists <- dist_between_airports[a1,]
    target_ind <- which(dists > 150)[1]
    new_df[[a1]] <- append(new_df[[a1]], colnames(dist_between_airports)[target_ind])
    new_df[[colnames(dist_between_airports)[target_ind]]] <- append(new_df[[colnames(dist_between_airports)[target_ind]]],
                                                                    a1)
    tracker <- rbind(tracker, c(a1, shared1, colnames(dist_between_airports)[target_ind]))
    target_ind2 <- which(dists > 150)[2]
    new_df[[a2]] <- append(new_df[[a2]], colnames(dist_between_airports)[target_ind2])
    new_df[[colnames(dist_between_airports)[target_ind2]]] <- append(new_df[[colnames(dist_between_airports)[target_ind2]]],
                                                                    a2)
    tracker <- rbind(tracker, c(a2, shared2, colnames(dist_between_airports)[target_ind2]))
  } else if(length(new_set) == 1){
    a1 <- new_set[[1]][1,1]
    b1 <- new_set[[1]][1,2]
    shared1 <- new_set[[1]][1,3]
    new_df[[a1]] <- new_df[[a1]][-which(new_df[[a1]] == shared1)]
    new_df[[shared1]] <- new_df[[shared1]][-which(new_df[[shared1]] == a1)]
    dists <- dist_between_airports[a1,]
    target_ind <- which(dists > 150)[1]
    new_df[[a1]] <- append(new_df[[a1]], colnames(dist_between_airports)[target_ind])
    new_df[[colnames(dist_between_airports)[target_ind]]] <- append(new_df[[colnames(dist_between_airports)[target_ind]]],
                                                                    a1)
    tracker <- rbind(tracker, c(a1, shared1, colnames(dist_between_airports)[target_ind]))
  }
}

ch <- data.frame(list(A = c("ATL", "ATL"), B = c("AGS", "AGS"), c=c("CLT", "DFW")))
ch
length(tapply(ch$B, ch$c, print))
length(ch$c)
length(ch$b)
tapply(ch, ch$c, unique)[[1]][1,]
