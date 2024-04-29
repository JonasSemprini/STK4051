# Read the sparse data
sparseData <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(sparseData) <- c('betaGT', 'y')
betaGT <- sparseData$betaGT
# Extract data from the dataframe
y <- sparseData$y

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
p <- 0.90
tau <- 80

# Calculate beta estimator for each y value
beta_hat_y <- calc_beta_hat(y, p, tau)

residuals = sum((beta_hat_y-betaGT)^2)

print(residuals)