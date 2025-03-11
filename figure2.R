source("cond_rej_prob.R")

set.seed(0)

# Script is run on a logarithmic scale for numerical stability

# Set up experiment
mu <- 0.5 
N <- 100  # Number of evaluations
big_N <- 100  # Final N
l_alpha <- -big_N * (mu)^2 / 2

# Generate sample
X <- rnorm(N, mean = mu)

# Create vectors to store test values
vec_z <- c()
SLRT <- c()

# Calculate test values
for (n in 1:N) {
  vec_z[n] <- cond_z(n, big_N, X, l_alpha, 1, TRUE)
  SLRT[n] <- sum(dnorm(X[1:n], mu, log = TRUE)) - sum(dnorm(X[1:n], 0, log = TRUE))
}

# Prepare plot figure
# Reset graphical device
graphics.off()

# Open a PNG export device
png("plot_with_legend.png", width = 1.2 * 8, height = 1.2 * 4, units = "in", res = 300)

# Adjust margins and scaling
par(mar = c(4, 5, 0.5, 3),
    cex.lab = 1.5,      
    cex.axis = 1.2,     
    lwd = 2)

rs_z <- c(l_alpha, vec_z)
rs_lr <- c(0, SLRT) + l_alpha

# Plot
plot(0:N, rs_z, type = "l", 
     ylab = "Log value of sequential test", 
     xlab = "n", 
     ylim = c(min(rs_z[1:N]), max(rs_z[1:N])), 
     xlim = c(0, N),
     lty = 1, 
     lwd = 2)

lines(0:N, rs_lr, col = "blue", lty = 2, lwd = 2)

legend("topleft", 
       legend = c("z-test", "LR"), 
       col = c("black", "blue"), 
       lty = c(1, 2), 
       lwd = 2, 
       cex = 1.5,         
       pt.cex = 1.5,      
       bty = "o",         
       box.lwd = 2,       
       inset = c(0.05, 0.05))  

dev.off()
