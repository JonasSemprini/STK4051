# Load data
data <- read.table("data/functionEstimationNN.ascii", header = FALSE, col.names = c("x", "y"))

# Sample a subset of the data
set.seed(123) # For reproducibility
subset_size <- 1000
data_subset <- data[sample(nrow(data), subset_size), ]

# Train-test split
train_ratio <- 0.8
train_indices <- sample(1:nrow(data_subset), train_ratio * nrow(data_subset))
train_data <- data_subset[train_indices, ]
test_data <- data_subset[-train_indices, ]

# Extract train and test data
x_train <- train_data$x
y_train <- train_data$y
x_test <- test_data$x
y_test <- test_data$y

# Network parameters
n_inputs <- 1
n_hidden <- 50
n_outputs <- 1

# Initialize weights and biases
alpha <- matrix(rnorm(n_inputs * n_hidden, sd = sqrt(2/n_inputs)), nrow = n_inputs, ncol = n_hidden)
alpha_0 <- runif(n_hidden, min = 0, max = 1)
beta <- matrix(rnorm(n_hidden * n_outputs, sd = sqrt(2/n_hidden)), nrow = n_hidden, ncol = n_outputs)
beta_0 <- runif(n_outputs, min = 0, max = 1)

# Hyperparameters
learning_rate <- 0.001
learning_rate_decay <- 1e-4
batch_size <- 5
n_epochs <- 50

# Train network with SGD
SSEseq <- MSEseq <- MSE_testseq <- NULL
iter <- 0
for (epoch in 1:n_epochs) {
  learning_rate <- learning_rate / (1 + epoch * learning_rate_decay)
  x_shuffled <- x_train
  y_shuffled <- y_train
  n_batches <- ceiling(length(x_train) / batch_size)
  batches <- split(1:length(x_train), rep(1:n_batches, each = batch_size, length.out = length(x_train)))
  
  for (batch in batches) {
    iter <- iter + 1
    x_batch <- x_shuffled[batch]
    y_batch <- y_shuffled[batch]
    
    # Forward propagation
    h_batch <- pmax(x_batch %*% alpha + alpha_0, 0)
    y_pred_batch <- h_batch %*% beta + beta_0
    
    # Loss calculation
    sse_batch <- sum((y_pred_batch - y_batch)^2)
    mse_batch <- sse_batch / length(batch)
    SSEseq <- c(SSEseq, sse_batch)
    MSEseq <- c(MSEseq, mse_batch)
    
    # Backward propagation
    d_y_sse_batch <- - 2 * (y_batch - y_pred_batch) / length(batch)
    d_beta <- t(h_batch) %*% d_y_sse_batch
    d_beta_0 <- colSums(d_y_sse_batch)
    d_h <- d_y_sse_batch %*% t(beta) * (h_batch > 0)
    d_alpha <- t(x_batch) %*% d_h
    d_alpha_0 <- colSums(d_h)
    
    # Update weights and biases
    alpha <- alpha - learning_rate * d_alpha
    alpha_0 <- alpha_0 - learning_rate * d_alpha_0
    beta <- beta - learning_rate * d_beta
    beta_0 <- beta_0 - learning_rate * d_beta_0
    
    # Forward propagation for test data
    h_test <- pmax(x_test %*% alpha + alpha_0, 0)
    y_pred_test <- h_test %*% beta + beta_0
    mse_test <- sum((y_pred_test - y_test)^2) / length(y_test)
    MSE_testseq <- c(MSE_testseq, mse_test)
  }
  
  # Print MSE for current epoch
  cat("Epoch:", epoch, "| MSE:", MSEseq[length(MSEseq)], "| Learning Rate:", learning_rate, "\n")
}

# Plot MSE for training and test data
# Plot MSE for training and test data with y-axis scaled from 0 to 1
pdf("MSE_train_test.pdf")
plot(MSEseq, type = "l", xlab = "Iteration", ylab = "MSE ", col = "black", ylim = c(0, 2))
lines(MSE_testseq, type = "l", col = "red")
legend("topright", legend = c("Training MSE", "Test MSE"), col = c("black", "red"), lty = 1)
grid()
dev.off()

pdf("True_v_Predict.pdf")
# Plot true vs. predicted values
plot(x_test, y_test, col = "blue", main = "True vs. Predicted Values", xlab = "x_test", ylab = "y")
points(x_test, y_pred_test, col = "red")
legend("topright", legend = c("True Data", "Predicted Values"), col = c("blue", "red"), lty = 1)
grid()
dev.off()
# Calculate residuals
residuals <- y_test - y_pred_test

# # Plot residuals vs. predicted values
# plot(y_pred_test, residuals, col = "blue", main = "Residuals vs. Predicted Values", xlab = "Predicted Values", ylab = "Residuals")
# abline(h = 0, col = "red")

# Histogram of residuals
pdf("Residual_histo.pdf")
hist(residuals, col = "darkblue", main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency")
grid()
dev.off()
# Scatterplot of residuals
# plot(y_pred_test, residuals, col = "blue", main = "Scatterplot of Residuals", xlab = "Predicted Values", ylab = "Residuals")
# abline(h = 0, col = "red")
