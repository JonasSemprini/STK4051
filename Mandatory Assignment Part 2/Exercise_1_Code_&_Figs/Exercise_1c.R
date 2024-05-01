# Define the standard normal density function
set.seed(123)
phi <- function(x) {
  return(dnorm(x))
}

# Define the target density function g(x)
g <- function(x) {
  ifelse(abs(x) <= 0.5, 1/3, exp(-abs(x) + 0.5) / 3)
}

# Rejection sampling function
rejection_sampling <- function(iterations) {
  accepted <- 0
  
  for (i in 1:iterations) {
    # Step 1: Sample U1, U2 from Uniform [0,1]
    U1 <- runif(1)
    U2 <- runif(1)
    
    # Step 2: Sample i from {1, 2, 3} with equal probability
    i <- sample(1:3, 1, prob = c(1/3, 1/3, 1/3))
    
    # Step 3: Generate proposal x_p based on i
    if (i == 1) {
      x_p <- U1 - 0.5
    } else if (i == 2) {
      x_p <- log(1 - U1) - 0.5
    } else {
      x_p <- -log(1 - U1) + 0.5
    }
    
    # Step 4: Accept or reject proposal based on U2 and the acceptance condition
    if (U2 < sqrt(2 * pi) * phi(x_p) / (3 * g(x_p))) {
      accepted <- accepted + 1
      # Here you can store or print the accepted proposals
    }
  }
  
  return(accepted / iterations)  # Return the average acceptance rate
}

# Set the number of iterations
iterations <- 10000

# Run rejection sampling and compute the average acceptance rate
average_acceptance_rate <- rejection_sampling(iterations)
print(paste("Average Acceptance Rate:", average_acceptance_rate))
