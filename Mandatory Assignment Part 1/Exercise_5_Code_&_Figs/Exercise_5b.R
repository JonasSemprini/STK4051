# Load data
dataset <- read.table("data/functionEstimationNN.ascii", header = FALSE, col.names = c("x", "y"))

# Sample a subset of the data
set.seed(123) # For reproducibility
subset_size <- 1000
subset <- dataset[sample(nrow(dataset), subset_size), ]

# Train-test split
train_ratio <- 0.8
train_indices <- sample(1:nrow(subset), train_ratio * nrow(subset))
train_set <- subset[train_indices, ]
test_set <- subset[-train_indices, ]

# Extract train and test data
x_train <- train_set$x
y_train <- train_set$y
x_test <- test_set$x
y_test <- test_set$y

# Network parameters
n_inputs <- 1
n_hidden <- 50
n_outputs <- 1

# Initialize weights and biases
weights_hidden <- matrix(rnorm(n_inputs * n_hidden, sd = sqrt(2/n_inputs)), nrow = n_inputs, ncol = n_hidden)
bias_hidden <- runif(n_hidden, min = 0, max = 1)
weights_output <- matrix(rnorm(n_hidden * n_outputs, sd = sqrt(2/n_hidden)), nrow = n_hidden, ncol = n_outputs)
bias_output <- runif(n_outputs, min = 0, max = 1)

# Hyperparameters
learning_rate <- 0.001
learning_rate_decay <- 1e-4
batch_size <- 50
n_epochs <- 200

# Train network with SGD
SSE_seq <- MSE_seq <- MSE_test_seq <- NULL
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
    hidden_output <- pmax(x_batch %*% weights_hidden + bias_hidden, 0) # ReLU activation
    y_pred_batch <- hidden_output %*% weights_output + bias_output
    
    # Loss calculation
    sse_batch <- sum((y_pred_batch - y_batch)^2)
    mse_batch <- sse_batch / length(batch)
    SSE_seq <- c(SSE_seq, sse_batch)
    MSE_seq <- c(MSE_seq, mse_batch)
    
    # Backward propagation
    d_y_sse_batch <- - 2 * (y_batch - y_pred_batch) / length(batch)
    d_weights_output <- t(hidden_output) %*% d_y_sse_batch
    d_bias_output <- colSums(d_y_sse_batch)
    d_hidden <- d_y_sse_batch %*% t(weights_output) * (hidden_output > 0)
    d_weights_hidden <- t(x_batch) %*% d_hidden
    d_bias_hidden <- colSums(d_hidden)
    
    # Update weights and biases
    weights_hidden <- weights_hidden - learning_rate * d_weights_hidden
    bias_hidden <- bias_hidden - learning_rate * d_bias_hidden
    weights_output <- weights_output - learning_rate * d_weights_output
    bias_output <- bias_output - learning_rate * d_bias_output
    
    # Forward propagation for test data
    hidden_output_test <- pmax(x_test %*% weights_hidden + bias_hidden, 0)
    y_pred_test <- hidden_output_test %*% weights_output + bias_output
    mse_test <- sum((y_pred_test - y_test)^2) / length(y_test)
    MSE_test_seq <- c(MSE_test_seq, mse_test)
  }
  
  # Print MSE for current epoch
  cat("Epoch:", epoch, "| MSE:", MSE_seq[length(MSE_seq)], "| Learning Rate:", learning_rate, "\n")
}

# Plot MSE for training and test data
# Plot MSE for training and test data with y-axis scaled from 0 to 2
pdf("MSE_train_test.pdf")
plot(MSE_seq, type = "l", xlab = "Iteration", ylab = "MSE ", col = "black", ylim = c(0, 0.2))
lines(MSE_test_seq, type = "l", col = "red")
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
residuals <- abs(y_test - y_pred_test)

# # Plot residuals vs. predicted values
# plot(y_pred_test, residuals, col = "blue", main = "Residuals vs. Predicted Values", xlab = "Predicted Values", ylab = "Residuals")
# abline(h = 0, col = "red")

# Histogram of residuals
pdf("Residual_histo.pdf")
hist(residuals, col = "lightblue", main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency")
grid()
dev.off()
# Scatterplot of residuals
# plot(y_pred_test, residuals, col = "blue", main = "Scatterplot of Residuals", xlab = "Predicted Values", ylab = "Residuals")
# abline(h = 0, col = "red")
