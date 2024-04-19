# Define the function to calculate p_i
calculate_pi <- function(y_i, squared_tau, p) {
  num <- dnorm(y_i, mean = 0, sd = 1) * p
  denom <- num + dnorm(y_i, mean = 0, sd = sqrt(squared_tau + 1)) * (1 - p)
  num / denom
}

# Define the EM algorithm function
EM_algorithm <- function(y, p_init, squared_tau_init, printing = FALSE, eps = 1e-10, max_iter = 1000) {
  iter <- 1
  converged <- FALSE
  p_prev <- p_init
  squared_tau_prev <- squared_tau_init
  
  while (!(converged) && iter < max_iter) {
    p_new <- mean(sapply(y, function(y_i) calculate_pi(y_i, squared_tau_prev, p_prev)))
    squared_tau_new <- sum(sapply(y, function(y_i) {
      pi_val <- calculate_pi(y_i, squared_tau_prev, p_prev)
      (1 - pi_val) * y_i^2
    })) / sum(sapply(y, function(y_i) 1 - calculate_pi(y_i, squared_tau_prev, p_prev))) - 1
    
    # Check for convergence
    if (abs(p_new - p_prev) + abs(squared_tau_new - squared_tau_prev) < eps) {
      converged <- TRUE
    }
    
    if (printing) {
      print(sprintf('iter: %2.0f, %f %f', iter, p_prev, squared_tau_prev))
    }
    
    # Update parameters
    p_prev <- p_new
    squared_tau_prev <- squared_tau_new
    
    iter <- iter + 1
  }
  
  c(p_new, squared_tau_new)
}

# Read the sparse data
sparseData <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(sparseData) <- c('betaGT', 'y')

# Extract data from the dataframe
y <- sparseData$y

# Initialize parameters
squared_tau_init <- 3
p_init <- 0.5

# Run the EM algorithm and collect results
values <- EM_algorithm(y, p_init, squared_tau_init, printing = TRUE)
p_opt <- values[1]
squared_tau_opt <- values[2]
