set.seed(2000)
# Define the target density function g(x)
g <- function(x) {
  ifelse(abs(x) <= 0.5, 1/3, exp(-abs(x) + 0.5) / 3)
}

# Function h(x)
h <- function(x) {
  return(x^2 * (x > 0))
}

# Rejection sampling function without NA estimates
# Rejection sampling function without NA estimates
rejection_sampling <- function(n_samples) {
  accepted_samples <- numeric(0)
  while (length(accepted_samples) == 0) {
    for (i in 1:n_samples) {
      accepted <- FALSE
      while (!accepted) {
        # Step 1: Sample U1, U2 from Uniform [0,1]
        U1 <- runif(1)
        U2 <- runif(1)
        
        # Step 2: Sample i from {1, 2, 3} with equal probability
        i <- sample(1:3, 1, prob = c(1/3, 1/3, 1/3))
        
        # Step 3: Generate proposal x_p based on i
        if (i == 1) {
          x_p <- U1 - 0.5
        } else if (i == 2) {
          x_p <- -log(1 - U1) + 0.5
        } else {
          x_p <- log(1 - U1) - 0.5
        }
        
        # Step 4: Accept or reject proposal based on U2 and the acceptance condition
        if (U2 < sqrt(2 * pi) * dnorm(x_p) / (3 * g(x_p))) {
          accepted_samples <- c(accepted_samples, x_p)
          accepted <- TRUE
        }
      }
    }
  }
  
  estimate <- mean(h(accepted_samples))
  return(estimate)
}




importance_sampling <- function(n_samples) {
  # Generate samples from the proposal distribution (standard normal)
  samples <- runif(n_samples)
  
  # Calculate importance weights
  weights <- sapply(samples, function(x) g(x) / h(x))
  
  # Normalize weights
  normalized_weights <- weights / sum(weights)
  
  # Compute estimate of the expectation
  estimate <- sum(samples * normalized_weights)
  
  return(estimate)
}
# Number of iterations
N <- 1000

# Results storage
rejection_estimates <- numeric(N)
importance_estimates <- numeric(N)

# Perform the numerical study for rejection sampling
for (i in 1:N) {
  rejection_estimates[i] <- rejection_sampling(1)
}

# Perform the numerical study for importance sampling
for (i in 1:N) {
  importance_estimates[i] <- importance_sampling(1)
}

# Compute mean estimates
rejection_mean <- mean(rejection_estimates)
importance_mean <- mean(importance_estimates)

rejection_sd <- sd(rejection_estimates, na.rm = TRUE)
importance_sd <- sd(importance_estimates)

# Print results
print("Rejection Sampling:")
print(paste("Mean Estimate:", rejection_mean))
print(paste("Standard Deviation:", rejection_sd))

print("\nImportance Sampling:")
print(paste("Mean Estimate:", importance_mean))
print(paste("Standard Deviation:", importance_sd))
