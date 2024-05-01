# Load required libraries
# library("dplyr")
set.seed(2000)
# Define the prior distribution for a (uniform)
prior_sample <- function(n_samples) {
  runif(n_samples, 0, 1)
}


# Likelihood function
likelihood <- function(particles, y, sigma_sq) {
  # Compute likelihood based on the observed class 'y'
  likelihood <- ifelse(y == 1, dnorm(particles, mean = 0, sd = sqrt(0.5^2)),
                       ifelse(y == 2, dnorm(particles, mean = 0, sd = sqrt(0.5^2)),
                              dnorm(particles, mean = 0, sd = sqrt(0.5^2))))
  
  # Return the likelihood
  return(likelihood)
}


# Sequential Monte Carlo algorithm
sequential_monte_carlo <- function(y, n_particles) {
  # Initialize particles with samples from the prior distribution
  particles <- prior_sample(n_particles)
  
  # Initialize weights
  weights <- rep(1/n_particles, n_particles)
  
  # Iterate over observations
  for (obs in y) {
    # Compute likelihood for each particle
    particle_likelihood <- likelihood(particles, obs)
    
    # Compute unnormalized weights
    unnormalized_weights <- weights * particle_likelihood
    
    # Normalize weights
    normalized_weights <- unnormalized_weights / sum(unnormalized_weights)
    
    # Resample particles
    resampled_indices <- sample.int(n_particles, size = n_particles, replace = TRUE, prob = normalized_weights)
    particles <- particles[resampled_indices]
    
    # Reset weights
    weights <- rep(1/n_particles, n_particles)
  }
  
  # Estimate for p(a | y)
  estimate <- mean(particles)
  
  return(estimate)
}

# Load data
data <- read.table("data/tgsim.ascii", header = FALSE)
colnames(data) <- c('y')
y <- data$y

# Number of particles
n_particles <- 1000
# Run sequential Monte Carlo algorithm
estimate <- sequential_monte_carlo(y, n_particles)
print(paste("Estimate of p(a | y):", estimate))


