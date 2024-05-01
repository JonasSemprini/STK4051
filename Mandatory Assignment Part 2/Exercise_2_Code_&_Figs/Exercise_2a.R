set.seed(2000)
# Parameters
a <- 0.85
lambda <- 0
sigma_sq <- 0.5^2

# Load data
data <- read.table("data/tgsim.ascii", header = FALSE)
colnames(data) <- c('y')
y <- data$y
n <- length(y)

# Initialize
n_particles <- 1000
particles <- matrix(0, nrow = n_particles, ncol = n)
weights <- rep(1/n_particles, n_particles)

# Initialize vectors to store estimates
E_xt <- numeric(n)
Var_xt <- numeric(n)

# SIS algorithm
for (t in 2:n) {
  # Propagate particles forward according to the state transition model
  particles[, t] <- a * particles[, t-1] + lambda + rnorm(n_particles, mean = 0, sd = sqrt(sigma_sq))
  
  # Sample epsilon from normal distribution
  epsilon <- rnorm(n_particles, mean = 0, sd = sqrt(sigma_sq))
  
  # Assign discrete values y_t based on intervals
  y <- ifelse(particles[, t] < -0.5, 1, ifelse(particles[, t] < 0.5, 2, 3))
  
  # Calculate importance weights
  likelihood <- ifelse(y == 1, dnorm(particles[, t], mean = 0, sd = sqrt(sigma_sq)),
                       ifelse(y == 2, dnorm(particles[, t], mean = 0, sd = sqrt(sigma_sq)),
                              dnorm(particles[, t], mean = 0, sd = sqrt(sigma_sq))))
  weights <- weights * likelihood
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Resampling
  indices <- sample(1:n_particles, n_particles, replace = TRUE, prob = weights)
  particles <- particles[indices, ]
  weights <- rep(1/n_particles, n_particles)
  
  # Calculate estimates
  E_xt <- colMeans(particles)
  Var_xt <- apply(particles, 2, var)
}

# Plotting
plot(E_xt, type = "l", col = "blue", xlab = "Time", ylab = "Expectation", main = "E(x_t | y_{1:t})")
plot(Var_xt, type = "l", col = "red", xlab = "Time", ylab = "Variance", main = "Var(x_t | y_{1:t})")
# lines(E_xt + sqrt(Var_xt), col = "red")
# lines(E_xt - sqrt(Var_xt), col = "darkgreen")