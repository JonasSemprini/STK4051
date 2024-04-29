# Read sparse data and rename columns
data_df <- read.table("data/sparseDataWithErrors.ascii", header = FALSE)
colnames(data_df) <- c('beta_GT', 'y')

# Extract data
y <- data_df$y

# Define the log-likelihood function
likelihood <- function(y, p, tau_sq) {
  fy <- p * dnorm(y, mean = 0, sd = 1) + (1 - p) * dnorm(y, mean = 0, sd = sqrt(tau_sq + 1))
  return(sum(log(fy)))
}

# Define the grid for p and tau_sq
p_grid <- seq(0.8, 1, length.out = 101)
tau_sq_grid <- seq(50, 130, length.out = 101)

# Calculate the log-likelihood values on the grid
z <- outer(p_grid, tau_sq_grid, Vectorize(function(p, tau_sq) likelihood(y, p, tau_sq)))

# Normalize the log-likelihood values
z <- max(z) / z

# Define contour levels
levels_ <- c(0.01, 0.1, 0.5, 0.75, 0.95, 0.98, 0.99, 0.9999, 1)

red_palette <- colorRampPalette(c("#67000D", "#A50F15", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2"))

# Plot the contour plot with the reversed custom red color palette
pdf("contour_plot.pdf")
#contour(tau_sq_grid, p_grid, z, levels = levels_, xlab = "squared_tau", ylab = "p", col = red_palette(length(levels_)))
filled.contour(tau_sq_grid, p_grid, z, ylab = 'p', xlab = expression(tau^2), levels = levels_, col = red_palette(length(levels_))) 

# Find the ML estimator and mark it in the plot
ML_idx <- which(z == max(z), arr.ind = TRUE)
points(tau_sq_grid[ML_idx[2]], p_grid[ML_idx[1]], col = "black", pch = 16)

# Add label for ML estimator coordinates
text(tau_sq_grid[ML_idx[2]], p_grid[ML_idx[1]], labels = paste("(", tau_sq_grid[ML_idx[2]], ",", p_grid[ML_idx[1]], ")"), pos = 3)
dev.off()









