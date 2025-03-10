# Function to compute conditional rejection probability of one-sided t-test
# for a given n, N, X, and alpha
cond_t <- function(n, N, X, alpha) {
  N_shape <- (N - 1) / 2  # Final shape parameter
  crit <- qbeta(1 - alpha, N_shape, N_shape) * 2 - 1  # Critical value
  beta <- sum(X[1:n]) / sqrt(n) / sqrt(sum(X[1:n]^2))  # Current stat
  
  if (n == N) {
    phi <- ifelse(beta > crit, 1, 0)
  } else {
    # For the rescaled Beta variable B_{N-n} on [-1,1],
    # the shape parameter is regularized to be at least a small positive number
    alpha_beta <- max(1e-7, (N - n - 1) / 2)
    
    # Define the integrand as a function of w in [0,1]
    integrand <- function(w) {
      x <- (sqrt(n) / sqrt(N - n) * beta * sqrt(w) - sqrt(N) / sqrt(N - n) * d) / sqrt(1 - w)
      
      # Map x from [-1,1] to u in [0,1]
      u <- (x + 1) / 2
      
      # F_{B_{N-n}}(x) is then given by the Beta CDF
      F_B <- pbeta(u, alpha_beta, alpha_beta)
      
      # f_W(w) is the density of W ~ Beta(n/2, (N-n)/2)
      f_w <- dbeta(w, n / 2, (N - n) / 2)
      
      return(F_B * f_w)
    }
    
    # Numerically integrate the integrand from 0 to 1
    result <- integrate(integrand, lower = 0, upper = 1)
    phi <- result$value
  }
  
  return(phi)
}

# Function to compute conditional rejection probability of one-sided z-test
# for a given n, N, X, and alpha
cond_z <- function(n, N, X, alpha, sd) {
  crit <- qnorm(1 - alpha)
  
  if (n == N) {
    phi <- ifelse(sqrt(N) * mean(X) / sd > crit, 1, 0)
  } else {
    phi <- 1 - pnorm((sqrt(N / (N - n)) * crit - 1 / sqrt(N - n) * sum(X[1:n]) / sd))
  }
  
  return(phi)
}
