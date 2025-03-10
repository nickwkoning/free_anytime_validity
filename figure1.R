source("cond_rej_prob.R")

set.seed(1)

# Set up experiment
alpha <- 0.05
sd <- 1  # Standard deviation
mu <- 0.3 * sd  # Effect size * sd
N <- 100 

# Generate sample
X <- rnorm(N, mu, sd)

# Create vectors to store test values
t_test <- numeric(N)
z_test <- numeric(N)

# Calculate conditional rejection probabilities
for (i in 1:N) {
  t_test[i] <- cond_t(i, N, X, alpha)
  z_test[i] <- cond_z(i, N, X, alpha, sd)
}

#Prepare plot figure
# Reset graphical device
graphics.off()

# Open a PNG export device
png("plot_z_t.png", width = 1.2 * 8, height = 1.2 * 4, units = "in", res = 300)

# Adjust margins and scaling
par(mar = c(4, 5, 0.5, 3),
    cex.lab = 1.5,      
    cex.axis = 1.2,     
    lwd = 2)

# Plot
plot(0:N, c(alpha, z_test), type = "l", 
     ylab = "Value of sequential test", 
     xlab = "n", 
     ylim = c(0, 1), 
     xlim = c(0, N),
     lty = 1, 
     lwd = 2)

lines(c(alpha, t_test), lty = 2, lwd = 2)

legend("topleft", 
       legend = c("z-test", "t-test"), 
       col = c("black", "black"), 
       lty = c(1, 2), 
       lwd = 2, 
       cex = 1.5)

dev.off()
