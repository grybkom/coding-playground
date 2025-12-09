library(tidyverse)

point <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")
X <- c(5, 2, 0, 5, 3, 2, 9, 3, 5, 9)
Y <- c(8, 4, 0, 6, 3, 1, 8, 6, 1, 3)
data <- data.frame(Point = point, X = X, Y = Y)

# create simple scatter plot
plot(data$X, data$Y)

# Euclidean distance function
euclidean_distance <- function(x1, x2, y1, y2) {
  distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
  return(distance)
}

# points P3, P5 and P7 as initial centroids
initial_cluster1 <- c(data$X[3], data$Y[3]) # P3
initial_cluster2 <- c(data$X[5], data$Y[5]) # P5
initial_cluster3 <- c(data$X[7], data$Y[7]) # P7

# Function to calculate the distance to all centroids
calculate_dist <- function(data, centroids) {
  # Dataframe to store results
  distance_data <- data.frame(Point = data$Point)
  
  # For loop to get distance from each point to each centroid
  for (i in seq_along(centroids)) {
    centroid <- centroids[[i]]
    centroid_x <- centroid[1]
    centroid_y <- centroid[2]
    
    # euclidean distance for each point
    distance <- euclidean_distance(data$X, centroid_x, data$Y, centroid_y)
    
    # add distances to distance_data
    distance_data[[paste0("Distance_to_C", i)]] <- distance
  }
  return(distance_data)
}

centroids <- list(
  initial_cluster1,
  initial_cluster2,
  initial_cluster3
)

# Calculate the distances
distance_data <- calculate_dist(data, centroids)

# Assign each point to a cluster
assign_cluster <- function(distanc_data) {
  distanc_data$Assigned_cluster <- apply(distance_data[,-1],1,which.min)
  return(distanc_data)
}

distance_data <- assign_cluster(distance_data)

# Calculate new centroids
calculate_centroids <- function(data, distance_data) {
  new_centroids <- list()
  
  for (k in unique(distance_data$Assigned_cluster)) {
    cluster_points <- data[distance_data$Assigned_cluster == k, ]
    
    # Mean of X and Y for current cluster
    new_centroid_x <- mean(cluster_points$X)
    new_centroid_y <- mean(cluster_points$Y)
    # Store results 
    new_centroids[[k]] <- c(new_centroid_x, new_centroid_y)
  }
  return(new_centroids)
}

# Recalculate centroids
new_centroids <- calculate_centroids(data, distance_data)

# Perform one iteration of K-Means
kmeans_iteration <- function(data, centroids) {
  # Calculate distances to updated centroids
  distance_df <- calculate_dist(data, centroids)
  
  # Assign clusters based on minimum distance
  distance_df <- assign_cluster(distance_data)
  
  # Recalculate centroids
  new_centroids <- calculate_centroids(data, distance_data)
  
  # Return updated cluster assignments and centroids
  list(distance_data = distance_data, centroids = new_centroids)
}


# Initialize centroids
centroids <- list(
  c(data$X[3], data$Y[3]), # Initial centroid for cluster 1 (P3)
  c(data$X[5], data$Y[5]), # Initial centroid for cluster 2 (P5)
  c(data$X[7], data$Y[7])  # Initial centroid for cluster 3 (P7)
)

# Run K-Means for a fixed number of iterations
max_iterations <- 10
for (i in 1:max_iterations) {
  cat("Iteration:", i, "\n")
  
  # Perform one iteration
  results <- kmeans_iteration(data, centroids)
  
  # Extract updated data and centroids
  distance_df <- results$distance_data
  new_centroids <- results$centroids
  
  # Check for convergence (if centroids do not change)
  if (all(sapply(1:length(centroids), function(k) all.equal(centroids[[k]], new_centroids[[k]])) == TRUE)) {
    cat("Converged after", i, "iterations.\n")
    break
  }
  
  # Update centroids for the next iteration
  centroids <- new_centroids
}

# Final results
print(distance_df)
print(centroids)