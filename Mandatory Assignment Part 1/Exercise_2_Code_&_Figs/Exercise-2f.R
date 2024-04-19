set.seed(123)

# Read sparse data
sparse_data <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(sparse_data) <- c('betaGT', 'y')

# Extract data
y <- sparse_data$y

# Define the log-likelihood function
log_likelihood <- function(params, y) {
  p <- params[1]
  tau_squared <- params[2]
  
  log_term <- sum(log(p * dnorm(y, mean = 0, sd = 1) + (1 - p) * dnorm(y, mean = 0, sd = sqrt(tau_squared + 1))))
  return(-log_term)  # Note: We negate the log-likelihood for minimization
}

# Set the parameters and data
params <- c(p = 0.5, tau_squared = 1)

# Define a function to compute the negative log-likelihood
neg_log_likelihood <- function(params) {
  return(log_likelihood(params, y))
}

# Use the `optim()` function to estimate the observed Fisher information
observed_fisher_information <- function(params, y) {
  hessian <- optim(params, neg_log_likelihood, method = "BFGS", hessian = TRUE, control = list(trace = 0))$hessian
  fisher_info <- -hessian
  return(fisher_info)
}

# Compute the observed Fisher information
fisher_info <- observed_fisher_information(params, y)
print(fisher_info)
