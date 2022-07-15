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

prop.x.75 <- prop.reacheable.points(lonlat_usa,75)
prop.x.100 <- prop.reacheable.points(lonlat_usa,100)

#save(prop.x.75, file = "/Users/shirley/Desktop/statcomp2022/BTRY_4520/Project4520/data/reachable_grids_within_75_miles.RData")
#save(prop.x.100,file = "/Users/shirley/Desktop/statcomp2022/BTRY_4520/Project4520/data/reachable_grids_within_100_miles.RData")

