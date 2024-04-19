# Define the function
f <- function(beta, gamma_val, p) {
  beta + gamma_val * sign(beta) * abs(beta) ^ (p - 1)
}

# Bisection method for finding the root of an expression
bisection_method <- function(y, gamma, p, init_beta1, init_beta2, eps = 1e-10, max_iter = 1000) {
  func <- function(beta) f(beta, gamma, p) - y
  check_roots <- func(init_beta1) * func(init_beta2)
  if (check_roots == 0) return(ifelse(func(init_beta1) == 0, init_beta1, init_beta2))
  if (check_roots > 0) stop('The initial guesses have the same sign of the function (not acceptable)')
  iter <- 0
  while (iter < max_iter) {
    mid_beta <- (init_beta1 + init_beta2) / 2 
    if (func(mid_beta) * func(init_beta1) < 0) init_beta2 <- mid_beta
    else if (func(mid_beta) * func(init_beta2) < 0) init_beta1 <- mid_beta
    else return(mid_beta)
    iter <- iter + 1
    if (abs(func(mid_beta)) < eps) return(mid_beta)
  }
  return(mid_beta)
}

# Load data
sparseData <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(sparseData) <- c('betaGT', 'y')

# Collecting the data from the dataframe
y <- sparseData$y
betaGT <- sparseData$betaGT

# We want to estimate beta_optimal for penalized regression
gamma_val <- 1
p_values <- c(1.1, 2, 100)
init_beta1_val <- -100
init_beta2_val <- 101
residuals <- c()
beta_p <- list()
# Finding the residuals for different p-values
for (p in p_values){
  estimation <- c()
  for (y_i in y){
    estimation <- c(estimation, bisection_method(y_i, gamma_val, p, init_beta1_val, init_beta2_val))
  }
  
  # comparing the estimation vs the ground truth
  beta_p <- c(beta_p, list(estimation))
  residuals <- c(residuals, sum((estimation - betaGT)^2))
}

residuals <- data.frame(p_values, residuals)

# Comparison to the MLE estimator, just equal to y
beta_MLE <- y
residual_MLE <- sum((beta_MLE - betaGT)^2)

# Estimate beta by using the residuals
y_list3 <- rep(list(y), 3)

beta_alternative <- list()
residuals_alternative <- rep(NA, 3) 
for (i in 1:3) {
  beta_alternative <- c(beta_alternative, list(unlist(y_list3[i]) - unlist(beta_p[i])))
  residuals_alternative[i] <- sum((unlist(beta_alternative[i]) - betaGT)^2)
}

residuals_alternative_table <- data.frame(p_values, residuals_alternative)

# Print residuals
print(residuals)
print(residual_MLE)
print(residuals_alternative_table)




