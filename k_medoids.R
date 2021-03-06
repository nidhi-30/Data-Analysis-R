#import required libraries
library(philentropy)

#load the dataset
dataset = read.csv("data_medoids.csv")
df <- dataset
print(df)

# read value of k from user
k <- readline(prompt="Enter value of k: ")
print(k)

# randomly select k datapoints as centroid
k_centroids <- df[sample(nrow(df), k), ]
print(k_centroids)
# get indexes of selected points
centroid_index <- as.numeric(rownames(k_centroids))
print(centroid_index)

# calculate dissimilrity matrix using manhattan distance method
dissimilarity_matrix <- distance(df, method = "manhattan")
print(dissimilarity_matrix)

# add column named Cluster to the data frame
df$Cluster <- 0
old_df <- df

# assign cluster class to selected centroid points in dataframe
for(i in 1:k){
  df[centroid_index[i], "Cluster"] <- i
}

# assign required values
iteration <- 0
cost <- 0
min_cost <- 0

set.seed(1234)

# loop for assigning clusters to the datapoits
# loop will run until 100 iterations done or cluster assignment do not change
while(iteration < 100){
  # get the datapoints and their indexes that are not selected as centroid from the dataframe
  k_datapoints <- df[-(centroid_index), ]
  points_index <- as.numeric(rownames(k_datapoints))
  print(points_index)
  
  # get distance between those datapoints that are not selected as centroids and the selected centroids from dissimilarity matrix
  for(i in 1:nrow(df)){
    if(i %in% points_index){
      # get minimum distance between datapoint at ith index and k clusters
      min_dist <- min(dissimilarity_matrix[i,c(centroid_index)])
      column_min <- which(dissimilarity_matrix[i,c(centroid_index)] == min_dist)
      # assign cluster class which has minimum distance from the ith datapoint to cluster column of datapoint
      df[i, "Cluster"] <- column_min
      
    }
  }
  print(df)
  
  # check previous clusters assignment with current cluster assignment
  # if current cluster assignment and previous cluster assignments are same then break the loop
  if(all.equal(old_df,df) == TRUE){
    break
  }
  # if current cluster assignment and previous cluster assignments are not same then update previous cluster assignments with current cluster assignment
  else{
    old_df <- df
  }
  
  # loop for calculating distance of the point and other points of same cluster to get new centroids
  for(i in 1:k){
    # get datapoints which has same cluster class as datapoint at ith index
    cluster_class <- df[df$Cluster== i,]
    
    # calculate sum of the point at ith index with other points
    dist_from_point <- 0
    for(j in 1:nrow(cluster_class)){
      distance_index <- as.numeric(rownames(cluster_class))
      dist_from_point <- dist_from_point+dissimilarity_matrix[j,c(distance_index)]
    }
    print(dist_from_point)
    # point which has minimum sum of distance from the other points of same cluster is assigned as new centroid
    index_distance_centroid <-  which(dist_from_point == min(dist_from_point))
    centroid_index[i] <- distance_index[index_distance_centroid]
  }
  
  # calculate numbers of iterations
  iteration <- iteration+1
  print("ITERATIONS")
  print(iteration)
}

# calculate silhouette wodth for each points of the dataset
print("Silhouette Width")
s_width <- c()
clusters <- c(1:k)
print(clusters)

# loop will iterate each datapoints to calculate silhouette width
for(i in 1:nrow(df)){
  
  # get dataponits which are not at ith index
  df_point <- df[-i,]
  # get datapoint which is at ith index
  selected_point <- df[i,]
  
  selected_point_index <- as.numeric(rownames(selected_point))
  print(selected_point_index)
  
  # get ponits which are in same cluster as ith datapoint
  same_cluster <- df[df$Cluster== selected_point$Cluster,]
  selected_cluster <- selected_point$Cluster
  print(selected_cluster)
  print(same_cluster)
  
  # calculate sum of distances from the datapoints which are in same cluster
  sum_dist <- 0
  for(j in 1:nrow(same_cluster)){
    same_cluster_point <- same_cluster[j,]
    same_cluster_index <- as.numeric(rownames(same_cluster_point))
    sum_dist <- sum_dist + dissimilarity_matrix[i,j]
  }
  
  # calculate average by the sum of distances with dividing number of points
  avg_distance <- sum_dist/(nrow(same_cluster-1))
  
  # calculate average distance of datapoint with other datapoints which are in different cluster
  avg_dist_other <- c()
  cluster_numbers <- as.numeric(k)-1
  other_clusters <- clusters[(which(clusters != selected_cluster))]
  print(other_clusters)
  
  # get average distance for other clusters
  for(c in 1:cluster_numbers){
    other_cluster <- df[df$Cluster== other_clusters[c],]
    sum_dist_other <- 0
    for(l in 1:nrow(other_cluster)){
      other_cluster_point <- other_cluster[j,]
      other_cluster_index <- as.numeric(rownames(other_cluster_point))
      sum_dist_other <- sum_dist_other + dissimilarity_matrix[i,l]
    }
    avg_dist_other <- sum_dist_other/nrow(other_cluster)
  }
  
  # get minimum average value among the clusters
  avg_dist_min <- min(avg_dist_other)
  
  # calculate silhouette width using calculated distances : (b-a)/max(b,a)
  s_width[i] <- (avg_dist_min-avg_distance)/max(avg_dist_min,avg_distance)
  
}

print(s_width)

# calculate average silhouette width of all datapoints
sum_silhouette_width <- 0
for(i in 1:length(s_width)){
  sum_silhouette_width <- sum_silhouette_width + s_width[i]
}
avg_silhouette_width <- sum_silhouette_width/nrow(df)
print(avg_silhouette_width)

#write csv file which contain new column for cluster class
write.csv(df, file = paste0("cluster_", k,".csv"))