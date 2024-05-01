# Set seed for reproducibility
set.seed(123)

# Define the standard normal density function
phi <- function(x) {
  return(dnorm(x))
}

g <- function(x) {
  ifelse(abs(x) <= 0.5, 1/3, exp(-abs(x) + 0.5) / 3)
}

# Importance sampling function
importance_sampling <- function(n_samples) {
  # Generate samples from the proposal distribution (standard normal)
  samples <- runif(n_samples)
  
  # Calculate importance weights
  weights <- sapply(samples, function(x) g(x) / phi(x))
  
  # Normalize weights
  normalized_weights <- weights / sum(weights)
  
  # Compute estimate of the expectation
  estimate <- sum(samples * normalized_weights)
  
  return(estimate)
}

# Number of samples to generate
n_samples <- 10000

# Run importance sampling
estimate <- importance_sampling(n_samples)

# Print estimate of the expectation
print(paste("Estimate of the expectation:", estimate))

