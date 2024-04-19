# Set seed for reproducibility
set.seed(123)

# Read sparse data
sparse_data <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(sparse_data) <- c('betaGT', 'y')

# Extract data
y <- sparse_data$y

# Define function to calculate p_i
calculate_pi <- function(y_i, tau_squared, p) {
  numerator <- dnorm(y_i, mean = 0, sd = 1) * p
  denominator <- numerator + dnorm(y_i, mean = 0, sd = sqrt(tau_squared + 1)) * (1 - p)
  numerator / denominator
}

# Define EM algorithm function
EM_algorithm <- function(y, p_init, tau_squared_init, eps = 1e-10, max_iter = 1000) {
  iter <- 1
  converged <- FALSE
  p_prev <- p_init
  tau_squared_prev <- tau_squared_init
  
  while (!(converged) && iter < max_iter) {
    p_new <- mean(sapply(y, function(y_i) calculate_pi(y_i, tau_squared_prev, p_prev)))
    tau_squared_new <- sum(sapply(y, function(y_i) {
      pi_val <- calculate_pi(y_i, tau_squared_prev, p_prev)
      (1 - pi_val) * y_i^2
    })) / sum(sapply(y, function(y_i) 1 - calculate_pi(y_i, tau_squared_prev, p_prev))) - 1
    
    # Check for convergence
    if (abs(p_new - p_prev) + abs(tau_squared_new - tau_squared_prev) < eps) {
      converged <- TRUE
    }
    
    # Update parameters
    p_prev <- p_new
    tau_squared_prev <- tau_squared_new
    
    iter <- iter + 1
  }
  
  c(p_new, tau_squared_new)
}

# Initialization
tau_squared_init <- mean(y)
p_init <- 0.5

# Run EM algorithm
values <- EM_algorithm(y, p_init, tau_squared_init)
p_opt <- values[1]
tau_squared_opt <- values[2]

# Bootstrap method
B <- 1000  # Increase to 1000
n <- length(y)
p_simulated <- numeric(B)
tau_squared_simulated <- numeric(B)

for (b in 1:B) {
  y_sample <- sample(y, n, replace = TRUE)
  values <- EM_algorithm(y_sample, p_init, tau_squared_init)
  p_simulated[b] <- values[1]
  tau_squared_simulated[b] <- values[2]
  if(b %% 100 == 0){
    print(b)
  }
}

# Plot results and save to PDF with blue color
pdf("bootstrap_results.pdf")
plot(tau_squared_simulated, p_simulated, xlab = "Estimated Tau Squared", ylab = "Estimated p", col = "blue")
dev.off()

# Bias
cat("Bias:\n")
print(mean(p_simulated) - p_opt)
print(mean(tau_squared_simulated) - tau_squared_opt)

# Standard error
cat("Standard error:\n")
print(sd(p_simulated))
print(sd(tau_squared_simulated))

# 95% confidence interval for p_simulated
p_conf_int <- quantile(p_simulated, c(0.025, 0.975))
cat("95% Confidence interval for p_simulated:\n")
print(p_conf_int)

# 95% confidence interval for tau_squared_simulated
tau_squared_conf_int <- quantile(tau_squared_simulated, c(0.025, 0.975))
cat("95% Confidence interval for tau_squared_simulated:\n")
print(tau_squared_conf_int)

