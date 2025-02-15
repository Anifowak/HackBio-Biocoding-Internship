# Define Parameters
K <- 1000  # Carrying capacity
N0 <- 10   # Initial population size
r <- 0.5   # Intrinsic growth rate
lag_phase <- 5  # Lag phase duration
exp_phase <- 10  # Exponential phase duration (not used in this example)

# Time range
t <- seq(0, 100, by = 0.1)

# Calculate logistic population growth curve with lag phase
calculate_logistic <- function(t, K, N0, r, lag_phase) {
                # Initialize population vector
                L <- numeric(length(t))
                
                # Loop through time points
                for (i in 1:length(t)) {
                                if (t[i] < lag_phase) {
                                                # Lag phase: population remains constant
                                                L[i] <- N0
                                } else {
                                                # Logistic growth after lag phase
                                                adjusted_time <- t[i] - lag_phase
                                                L[i] <- K / (1 + ((K - N0) / N0) * exp(-r * adjusted_time))
                                }
                }
                return(L)
}

# Calculate population growth curve
population_growth <- calculate_logistic(t, K, N0, r, lag_phase)

# Plot population growth curve
plot(t, population_growth, type = "l", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Population Size",
     main = "Logistic Population Growth with Lag Phase")
abline(h = K, col = "red", lty = 2, lwd = 1.5)  # Add a horizontal line for carrying capacity
legend("bottomright", legend = c("Population", "Carrying Capacity (K)"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1.5))