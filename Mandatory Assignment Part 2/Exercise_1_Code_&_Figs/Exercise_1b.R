# Define the log-likelihood function
log_likelihood <- function(x) {
  return(log(1/(2*pi)) - 0.5 * x^2)
}

# Define linearization functions
linearization_minus_1 <- function(x) {
  return(log(1/(2*pi)) + 0.5 + x)
}

linearization_0 <- function(x) {
  return(rep(log(1/(2*pi)), length(x)))
}

linearization_plus_1 <- function(x) {
  return(log(1/(2*pi)) + 0.5 - x)
}

# Define function to find intersection point between two lines
find_intersection <- function(a1, b1, a2, b2) {
  x_intersect <- (b2 - b1) / (a1 - a2)
  y_intersect <- a1 * x_intersect + b1
  return(c(x_intersect, y_intersect))
}

# Generate x values
x_values <- seq(-3, 3, length.out = 1000)

# Compute linearization values
linearization_minus_1_values <- linearization_minus_1(x_values)
linearization_0_values <- linearization_0(x_values)
linearization_plus_1_values <- linearization_plus_1(x_values)

# Find intersection points between linearization lines
intersection_minus_1_0 <- find_intersection(1, log(1/(2*pi)) + 0.5, 0, log(1/(2*pi)))
intersection_0_plus_1 <- find_intersection(-1, log(1/(2*pi)), 0, log(1/(2*pi)) + 0.5)
intersection_minus_1_plus_1 <- find_intersection(1, log(1/(2*pi)) + 0.5, -1, log(1/(2*pi)) + 0.5)

# Plot
plot(x_values, log_likelihood(x_values), type='l', col='blue', lwd=2, ylim=c(-5, 0),
     xlab='x', ylab='Log-Likelihood', main='Bounding Function for Log-Likelihood of Standard Normal Distribution')
lines(x_values, linearization_minus_1_values, col='red', lty='dashed', lwd=2)
lines(x_values, linearization_0_values, col='green', lty='dashed', lwd=2)
lines(x_values, linearization_plus_1_values, col='orange', lty='dashed', lwd=2)
points(intersection_minus_1_0[1], intersection_minus_1_0[2], col='black', pch=19)
points(-intersection_minus_1_0[1], intersection_minus_1_0[2], col='black', pch=19)
#points(intersection_0_plus_1[1], intersection_0_plus_1[2], col='black', pch=19)
points(intersection_minus_1_plus_1[1], intersection_minus_1_plus_1[2], col='black', pch=19)

abline(v = -0.5, col='black', lty='dotted')
abline(v = 0.5, col='black', lty='dotted')
legend('topright', legend=c('Log-Likelihood', 'Linearization at -1', 'Linearization at 0', 'Linearization at 1', 'Vertical Lines at -0.5 and 0.5', 'Intersection Points'),
       col=c('blue', 'red', 'green', 'orange', 'black', 'black'), lty=c(1, 2, 2, 2, 0, NA), pch=c(NA, NA, NA, NA, NA, 19))
grid()
