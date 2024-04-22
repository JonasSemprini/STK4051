# Function to calculate P(C_i=1 | y_i=y)
calc_cond_prob <- function(y, p, tau) {
  num <- p * dnorm(y, 0, 1)
  den <- num + (1 - p) * dnorm(y, 0, tau + 1)
  p_0 <- num / den
  return(1 - p_0)
}

# Function to calculate E(beta | y_i=y)
calc_expectation <- function(y, tau) {
  e <- y * tau / (tau + 1)
  return(e)
}

# Function to calculate beta estimator given y_i=y
calc_beta_hat <- function(y, p, tau) {
  p_c1 <- calc_cond_prob(y, p, tau)
  e <- calc_expectation(y, tau)
  beta_hat <- e * p_c1
  return(beta_hat)
}

# Generate sequence of y values
y_values <- seq(-5, 5, length.out = 501)
p <- 0.90
tau <- 80

# Calculate beta estimator for each y value
beta_hat_y <- calc_beta_hat(y_values, p, tau)

# Plotting
pdf("estimator_plot.pdf")  # Open PDF device
plot(y_values, beta_hat_y, type = "l", col = "blue", lwd = 2, xlab = "y", ylab = expression(paste("Estimator of"," ", beta)), ylim = range(beta_hat_y))
legend("topright", legend = "Estimator", col = "blue", lty = 1, lwd = 2)
grid()
dev.off()  # Close PDF device
