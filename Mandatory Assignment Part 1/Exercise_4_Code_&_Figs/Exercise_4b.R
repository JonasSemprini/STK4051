set.seed(2000)
# Read city data
city_data <- read.table("data/optimalTransport.ascii", header = FALSE)
colnames(city_data) <- c("x", "y")

# Function to calculate total journey distance
calculate_journey_distance <- function(route, distances) {
  total <- sum(distances[cbind(route[-length(route)], route[-1])])
  total <- total + distances[route[length(route)], route[1]]  # Return to starting point
  return(total)
}

# Function to perform TABU search
find_optimal_route <- function(distances, max_iterations, tabu_list_length) {
  num_cities <- nrow(distances)  # Number of cities
  theta <- c(1, sample(2:num_cities), 1)# Random order and returns to the starting city
  d <- as.vector(as.matrix(distances))
  initial_distance <- calculate_journey_distance(theta, distances)  # Total distance of the initial route
  optimal_distance <- initial_distance  # Initial optimal distance
  distance_sequence <- initial_distance  # Sequence of distances
  num_swaps <- num_cities * (num_cities - 1) / 2 - (num_cities - 1)  # Number of iterations for neighbor search
  search_table <- combn(2:num_cities, 2)  # Table of neighbor search
  tabu_table <- numeric(0)  # Tabu table
  for (iteration in 1:max_iterations) {
    optimal_distance2 <- optimal_distance + 10
    for (i in 1:num_swaps) {
      if (!(i %in% tabu_table)) {
        swap_index1 <- search_table[1, i]
        swap_index2 <- search_table[2, i]
        new_route <- theta
        new_route[c(swap_index1, swap_index2)] <- theta[c(swap_index2, swap_index1)]
        new_distance <- calculate_journey_distance(new_route, distances)
        if (new_distance < optimal_distance2) {
          optimal_distance2 <- new_distance
          swap_index <- i
        }
      }
    }
    theta[c(search_table[1, swap_index], search_table[2, swap_index])] <- theta[c(search_table[2, swap_index], search_table[1, swap_index])]
    optimal_distance <- optimal_distance2
    distance_sequence <- c(distance_sequence, optimal_distance)
    tabu_table <- c(tabu_table, swap_index)
    if (length(tabu_table) > tabu_list_length) {
      tabu_table <- tabu_table[-1]
    }
    if (optimal_distance < initial_distance) {
      optimal_route <- theta
      initial_distance <- optimal_distance
    }
  }
  return(list(optimal_route = optimal_route, shortest_distance = initial_distance, distance_sequence = distance_sequence))
}


# Calculate distances between cities
city_distances <- as.matrix(dist(city_data))

# Set parameters for TABU search
max_iterations <- 1000
tabu_list_length <- 40

# Run TABU search
result <- find_optimal_route(city_distances, max_iterations, tabu_list_length)
optimal_route <- result$optimal_route
shortest_distance <- result$shortest_distance
distance_sequence <- result$distance_sequence

# Print the results
cat("Shortest distance:", shortest_distance, "\n")
cat("Optimal route:\n")
print(optimal_route)
cat("Number of iterations to find optimal route:", which.min(distance_sequence), "\n")

pdf("opt_tabu.pdf")
plot(city_data, main = "TABU - TSP", col = "red")
lines(city_data[optimal_route, 1], city_data[optimal_route, 2], col = "blue")
points(city_data[1, ], col = "black", pch = 16)
legend("topright", legend = c("Optimal Tour", "Start"), col = c("blue", "black"), pch = c(NA, 16),  lty=c(1, NA))
grid()
dev.off()

# Plot time series of the shortest distance found over iterations
optimal_route_iteration <- which.min(distance_sequence)
reasonable_plot_range <- min(optimal_route_iteration + 1000, max_iterations)
pdf("distance_iter.pdf")
plot.ts(distance_sequence[1:reasonable_plot_range], xlab = "Iterations", ylab = "Distance", col = "darkgreen", ylim = c(3, max(distance_sequence)))
legend("topright", legend = c("Optimal Distance"), col = c("darkgreen"), lty = 1, cex = 0.8)
grid()
dev.off()




# Seed 2000

# > cat("Shortest distance:", shortest_distance, "\n")
# Shortest distance: 3.776257 
# > cat("Optimal route:\n")
# Optimal route:
#   > print(optimal_route)
# [1]  1 19 13  3 16 11  6  2  5 10 20  7  4 21  8 12  9 15 18 17 17  1
# > cat("Number of iterations to find optimal route:", which.min(distance_sequence), "\n")
# Number of iterations to find optimal route: 927 

#Random seed

# > cat("Shortest distance:", shortest_distance, "\n")
# Shortest distance: 3.732432 
# > cat("Optimal route:\n")
# Optimal route:
#   > print(optimal_route)
# [1]  1 14  3 18 18 17 17  9  9 15 12  8 21  4  7 20 10  6  2  5 19  1
# > cat("Number of iterations to find optimal route:", which.min(distance_sequence), "\n")
# Number of iterations to find optimal route: 900 

#All time best random seed 

# > cat("Shortest distance:", shortest_distance, "\n")
# Shortest distance: 3.581809 
# > cat("Optimal route:\n")
# Optimal route:
#   > print(optimal_route)
# [1]  1 14 13  3 16 11  6  2  5  7  4  21  12  9  8  15  18  10  17  20  19
# > cat("Number of iterations to find optimal route:", which.min(distance_sequence), "\n")
# Number of iterations to find optimal route: 292 