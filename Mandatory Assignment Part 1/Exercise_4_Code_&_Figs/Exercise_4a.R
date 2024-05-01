library(ggplot2)

# Read data
optimalTransport <- read.table("data/optimalTransport.ascii", header = FALSE)
colnames(optimalTransport) <- c('x', 'y')

set.seed(124)
# Define neighborhood function (swap two cities)
swap_cities <- function(tour) {
  n <- length(tour)
  i <- sample(2:(n - 1), 1)
  j <- sample(setdiff(1:n, i), 1)
  new_tour <- tour
  new_tour[c(i, j)] <- new_tour[c(j, i)]
  return(new_tour)
}

# Simulated annealing algorithm
simulated_annealing <- function(distances, initial_temp, cooling_rate, max_iter) {
  # Define objective function (total traveling time)
  total_time <- function(tour, distances) {
    total <- 0
    n <- length(tour)
    for (i in 1:(n - 1)) {
      total <- total + distances[tour[i], tour[i + 1]]
    }
    total <- total + distances[tour[n], tour[1]]  # Return to starting city
    return(total)
  }
  
  n <- nrow(distances)
  current_tour <- sample(1:n)
  best_tour <- current_tour
  best_time <- total_time(current_tour, distances)
  current_temp <- initial_temp
  
  for (iter in 1:max_iter) {
    new_tour <- swap_cities(current_tour)
    # Calculate total time
    delta <- total_time(new_tour, distances) - total_time(current_tour, distances)
    
    if (delta < 0 || runif(1) < exp(-delta / current_temp)) {
      current_tour <- new_tour
      current_time <- total_time(new_tour, distances)
      if (current_time < best_time) {
        best_tour <- new_tour
        best_time <- current_time
      }
    }
    
    current_temp <- current_temp * cooling_rate
  }
  
  return(list(best_tour = best_tour, best_time = best_time))
}

# Calculate distances matrix (Euclidean distance between cities)
distances <- as.matrix(dist(cbind(optimalTransport$x, optimalTransport$y)))

# Set parameters
initial_temp <- 10
cooling_rates <- seq(0.7, 0.99, by = 0.01)  # Range of cooling rates
max_iter <- 10000

# Store results
results <- list()

# Run simulated annealing for each cooling rate
for (cooling_rate in cooling_rates) {
  result <- simulated_annealing(distances, initial_temp, cooling_rate, max_iter)
  best_tour <- result$best_tour
  best_time <- result$best_time
  results[[as.character(cooling_rate)]] <- list(tour = best_tour, total_time = best_time)
}

# Find the best cooling rate
best_result <- which.min(sapply(results, function(x) x$total_time))
best_cooling_rate <- as.numeric(names(results)[best_result])

# Output best tour and total time for the best cooling rate
cat("Best Cooling Rate:", best_cooling_rate, "\n")
cat("Best Tour:", results[[as.character(best_cooling_rate)]]$tour, "\n")
cat("Best Total Distance:", results[[as.character(best_cooling_rate)]]$total_time, "\n")

# Plot the most optimal trip
optimal_path <- c(results[[as.character(best_cooling_rate)]]$tour, results[[as.character(best_cooling_rate)]]$tour[1])
cities <- optimalTransport[optimal_path, ]

pdf("opt_annealing.pdf")
ggplot() +
  geom_path(data = cities, aes(x = x, y = y), color = "blue") +
  geom_point(data = cities, aes(x = x, y = y), color = "red") +
  geom_text(data = cities, aes(x = x, y = y, label = rownames(cities)), hjust = -0.2, vjust = 0.5) +
  ggtitle("Most Optimal Tour") +
  theme_minimal()
dev.off()
# Plot distances with respect to cooling rates
data <- data.frame(cooling_rates = as.numeric(names(results)),
                   total_time = sapply(results, function(x) x$total_time))

pdf("opt_cooling.pdf")
ggplot(data, aes(x = cooling_rates, y = total_time)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(x = "Cooling Rate", y = "Distance") +
  ggtitle("Distances with Respect to Cooling Rate") +
  theme_minimal()

dev.off()
#Random seed
# Best Cooling Rate: 0.77 
# > cat("Best Tour:", results[[as.character(best_cooling_rate)]]$tour, "\n")
# Best Tour: 17 9 15 18 12 8 21 4 7 20 10 5 2 6 11 16 3 13 14 19 1 
# > cat("Best Total Time:", results[[as.character(best_cooling_rate)]]$total_time, "\n")
# Best Total Distance: 3.770697 


#Seed(124)
# Best Cooling Rate: 0.97 
# > cat("Best Tour:", results[[as.character(best_cooling_rate)]]$tour, "\n")
# Best Tour: 2 11 16 3 13 14 19 1 17 9 15 18 12 8 21 4 7 20 10 5 6 
# > cat("Best Total Time:", results[[as.character(best_cooling_rate)]]$total_time, "\n")
# Best Total Distance: 3.902836 

