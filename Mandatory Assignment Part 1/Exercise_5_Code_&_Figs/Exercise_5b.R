# Read data
sparseData <- read.table("data/functionEstimationNN.ascii", header = FALSE)
colnames(sparseData) <- c('x', 'y', 'fGT')

# Extract data
x <- sparseData$x
y <- sparseData$y
fGT <- sparseData$fGT
n <- length(x)

# Define ReLU activation function
ReLU <- function(x) {
  return(ifelse(x > 0, x, 0))
}

# Define derivative of ReLU activation function
dReLU <- function(x) {
  return(ifelse(x > 0, 1, 0))
}

# Define model architecture
f <- function(x, alpha, beta) {
  result <- sum(beta * ReLU(alpha * x))
  return(result)
}

# Define loss function (Sum of Squared Errors)
loss <- function(x, y, alpha, beta) {
  f_pred <- f(x, alpha, beta)
  return(sum((y - f_pred)^2))
}

# Initialize parameters
set.seed(123) # For reproducibility
alpha <- runif(50, min = -1, max = 1) # Initialize alpha parameters
beta <- runif(51, min = -1, max = 1) # Initialize beta parameters (including intercept)

# Hyperparameters
learning_rate <- 0.01
epochs <- 100

# SGD training
for (epoch in 1:epochs) {
  for (i in 1:n) {
    # Forward pass
    pred <- f(x[i], alpha, beta)
    
    # Backward pass (Compute gradients)
    dL_dbeta <- -2 * (y[i] - pred) * ReLU(alpha * x[i])
    dL_dalpha <- -2 * (y[i] - pred) * beta * dReLU(alpha * x[i]) * x[i]
    
    # Update parameters
    beta <- beta - learning_rate * dL_dbeta
    alpha <- alpha - learning_rate * dL_dalpha
  }
  
  # Compute and print loss
  current_loss <- loss(x, y, alpha, beta)
  cat("Epoch:", epoch, "Loss:", current_loss, "\n")
}

# Test the model
predictions <- sapply(x, function(xi) f(xi, alpha, beta))
