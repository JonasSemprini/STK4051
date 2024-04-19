# import numpy as np
# from scipy.optimize import minimize
# from numdifftools import Hessian


# # Set seed for reproducibility
# np.random.seed(123)

# # Read sparse data
# sparse_data = np.loadtxt("data/sparseDataWithErrors.ascii")

# # Extract data
# y = sparse_data[:, 1]


# # Define the log-likelihood function
# def log_likelihood(params, y):
#     p, tau_squared = params
#     log_term = np.sum(
#         np.log(
#             p * np.exp(-0.5 * (y**2)) / np.sqrt(2 * np.pi)
#             + (1 - p)
#             * np.exp(-0.5 * (y**2) / (tau_squared + 1))
#             / np.sqrt(2 * np.pi * (tau_squared + 1))
#         )
#     )
#     return -log_term


# def neg_log_likelihood(params):
#     p, tau_squared = params
#     if not (0 <= p <= 1 and tau_squared >= 0):
#         return np.inf
#     return -log_likelihood(params, y)

# # Use the `minimize()` function with parameter bounds
# def observed_fisher_information(params):
#     result = minimize(neg_log_likelihood, params, method='L-BFGS-B', bounds=[(0, 1), (0, None)])
#     if not result.success:
#         print("Optimization did not converge:", result.message)
#         return None
#     hessian_matrix = Hessian(neg_log_likelihood)(result.x)
#     fisher_info = -np.linalg.inv(hessian_matrix)
#     return fisher_info

# # Compute the observed Fisher information
# params_init = [0.5, 3]  # Initial guess for parameters
# fisher_info = observed_fisher_information(params_init)
# if fisher_info is not None:
#     print("Observed Fisher Information:")
#     print(fisher_info)
# else:
#     print("Unable to compute observed Fisher information.")
import numpy as np
from scipy.optimize import minimize

# Define the log-likelihood function
def log_likelihood(params, y):
    p, tau_squared = params
    log_term = np.sum(np.log(p * np.exp(-0.5 * y**2) / np.sqrt(2 * np.pi) +
                           (1 - p) * np.exp(-0.5 * y**2 / (tau_squared + 1)) / np.sqrt(2 * np.pi * (tau_squared + 1))))
    return -log_term

# Define a function to compute the negative log-likelihood
def neg_log_likelihood(params, y):
    return log_likelihood(params, y)

# Use the `minimize()` function to estimate the observed Fisher information
def observed_fisher_information(params, y):
    res = minimize(neg_log_likelihood, params, args=(y,), method="L-BFGS-B", bounds=[(0, 1), (0, None)])
    hessian = res.hess_inv
    if not res.success:
        print("Optimization failed:", res.message)
    if np.any(np.isnan(hessian)):
        print("Hessian matrix contains NaN values")
    fisher_info = -hessian
    return fisher_info

# Set the seed for reproducibility
np.random.seed(123)

# Generate sample data
y = np.random.normal(size=1000)

# Set initial parameters
params_init = [0.5, 3]  # Initial guess for p and tau_squared

# Compute the observed Fisher information
fisher_info = observed_fisher_information(params_init, y)
print("Observed Fisher Information:")
print(fisher_info)
