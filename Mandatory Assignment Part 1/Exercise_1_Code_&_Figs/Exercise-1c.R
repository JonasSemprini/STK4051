# Define parameters
beta <- seq(-5, 5, length = 101)
gamma_list <- c(1, 0.2)
p_list <- c(1.1, 2, 5, 100)
colors <- c("black", rainbow(length(p_list) - 1, start = 0.2, end = 0.7))

# Define the function
f <- function(beta, gamma, p) {
  beta + gamma * sign(beta) * abs(beta) ^ (p - 1)
}

# Plotting function
plot_function <- function(gamma, title) {
  plot(beta, rep(0, length(beta)), type = 'n', ylim = c(-8, 8), 
       xlim = c(-5.1, 5.1), xlab = expression(beta), lwd = 2, main = title, ylab = expression(paste("f"["p, y"], "(", beta, ")")))
  for (i in 1:length(p_list)) {
    lines(beta, f(beta, gamma, p_list[i]), col = colors[i], lwd = 2)
  }
  legend('topleft', legend = paste('p =', p_list), col = colors, lwd = 2, bty = 'n', title = "Values of p", xjust = 0, yjust = 1, inset = c(0, 0.05))
  lines(beta, beta, col = 'red')
  text(mean(range(beta)), max(beta) * 1.1, labels = expression(paste("f", "(", beta, ")", " = ", beta)), pos = 3, col = 'red')
}

# Set the size of the plot
options(repr.plot.width = 12, repr.plot.height = 8)

# Plotting for each value of gamma
eq1 <- bquote(bold(gamma == .(gamma_list[1])))
eq2 <- bquote(bold(gamma == .(gamma_list[2])))

plot_function(gamma_list[1], eq1)
plot_function(gamma_list[2], eq2)

# Save figures as PDF
# pdf(file = "plots.pdf")
# plot_function(gamma_list[1], eq1)
# plot_function(gamma_list[2], eq2)
# dev.off()









