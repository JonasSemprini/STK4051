# Define the log-likelihood function
log_likelihood <- function(y, p, tau_squared) {
  sum(log(
    p * dnorm(y, mean = 0, sd = 1) +
      (1 - p) * dnorm(y, mean = 0, sd = sqrt(tau_squared + 1))
  ))
}

# Load the data
sparse_data <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(sparse_data) <- c('betaGT', 'y')

# Extract y data
y <- sparse_data$y

# Define a grid of values for p and tau_squared
p_values <- seq(0.01, 0.99, length.out = 100)
tau_squared_values <- seq(0.01, 10, length.out = 100)
z <- outer(p_values, tau_squared_values, Vectorize(function(p, tau_squared) {
  log_likelihood(y, p, tau_squared)
}))

# Plot the log-likelihood function
library(lattice)
wireframe(z ~ p_values * tau_squared_values,
          xlab = "p", ylab = "tau_squared", zlab = "Log-Likelihood",
          main = "Log-Likelihood Function")
