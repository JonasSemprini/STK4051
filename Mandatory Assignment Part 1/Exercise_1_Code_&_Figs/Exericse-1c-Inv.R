# Define parameters
beta <- seq(-5, 5, length = 101)
gamma_list <- c(1, 0.2)
p_list <- c(1.1, 2, 5, 100)
colors <- c("black", rainbow(length(p_list) - 1, start = 0.2, end = 0.7))

# Define the function
f <- function(beta, gamma, p) {
  beta + gamma * sign(beta) * abs(beta) ^ (p - 1)
}

# Plotting function for the inverse function
inverse_function <- function(title, gamma, filename) {
  pdf(file = filename)  # Open a PDF file with the specified filename
  plot(beta, rep(0, length(beta)), type = 'n', ylim = c(-8, 8), 
       xlim = c(-5.1, 5.1), xlab = expression(beta), lwd = 2, main = title, ylab = expression(paste("f"["p, y"], "(", beta, ")")))
  for (i in 1:length(p_list)) {
    lines(f(beta, gamma, p_list[i]), beta, col = colors[i], lwd = 2)
  }
  legend('topleft', legend = paste('p =', p_list), col = colors, lwd = 2, bty = 'n', title = "Values of p", xjust = 0, yjust = 1, inset = c(0, 0.05))
  lines(beta, beta, col = 'red')
  text(mean(range(beta)), max(beta) * 1.1, labels = expression(paste("f", "(", beta, ")", " = ", beta)), pos = 3, col = 'red')
  dev.off()  # Close the PDF file
}

# Set the size of the plot
options(repr.plot.width = 12, repr.plot.height = 8)

# Plotting the inverse function for each value of gamma
for (gamma in gamma_list) {
  eq <- bquote(bold(paste(gamma == .(gamma))))
  inverse_function(eq, gamma, paste0("inverse_function_", gamma, ".pdf"))
}




