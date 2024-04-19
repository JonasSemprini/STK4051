# Define the function
f <- function(beta, gamma, p) {
  beta + gamma * sign(beta) * abs(beta) ^ (p - 1)
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
# Test bisection method
gamma_val <- 1
p_values <- c(1.1, 2, 100)
y_values <- seq(-5, 5, length = 101)
init_beta1_val <- -5
init_beta2_val <- 4.9

# Plotting function
plot_function <- function(p) sapply(y_values, function(y_i) bisection_method(y_i, gamma_val, p, init_beta1_val, init_beta2_val))

# Plotting
plot_colors <- rainbow(length(p_values))
plot_legend <- paste('p =', p_values)

pdf("bisection_plots.pdf")
plot(y_values, plot_function(p_values[1]), type='l', col = plot_colors[1], ylim = c(-5, 5), 
     xlim = c(-5.1, 5.1), lwd = 2, main = expression(paste("Optimal"," ", beta)), xlab = 'y', ylab = expression(beta["opt"]))
for (i in 2:length(p_values)) lines(y_values, plot_function(p_values[i]), col = plot_colors[i], lwd = 2)
legend('topleft', legend = plot_legend, col = plot_colors, lwd = 2, bty = 'n', title = "Values of p", xjust = 0, yjust = 1, inset = c(0, 0.05))
dev.off()

