library(Project4520)
continent_df

dist_between_airports <- matrix(0, nrow = nrow(continent_df), n = nrow(continent_df))

n = nrow(continent_df)
for(i in 1:n){
  for(j in 1:n){
    lonlat1 <- as.matrix(continent_df[i, 2:3], nrow = 1)
    lonlat2 <- as.matrix(continent_df[j, 2:3], nrow = 1)
    dist <- distfun(lonlat1, lonlat2)
    dist_between_airports[i,j] <- dist
  }
}
rownames(dist_between_airports) <- continent_df$airport_code
colnames(dist_between_airports) <- continent_df$airport_code
save(dist_between_airports,
     file = "/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/dist_between_airports.RData")

continent_df[which(continent_df$airport_code == "SYR"),]
dist_between_airports[which(rownames(dist_between_airports) == "SYR"),]
dist_between_airports["SYR",]
