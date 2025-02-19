# Growth equation:: P(t) = K / (1 +((K - P0) / p0)* e-rt

# where:P(t) = population at time t, P0 = initial population, K = carrying capacity, r = growth rate, t = time

# To find the time ttt when the population reaches 80% of K (i.e., 0.8K)
# 0.8K = K / (1 +((K - P0) / p0) * e-rt


# Taking the natural logarithm and solving for ttt gives:
# t = −ln (K / 0.8K−1) / r

# Function to calculate time to reach 80% of carrying capacity
time_to_80_growth <- function(P0, K, r) {
                
                # calculate the time
                t <- -log((K / (0.8 * K)) - 1) / r
                
                return(t)
}

# Example Usage
P0 <- 10    # Initial population
K <- 0.8   # Carrying capacity
r <- 0.2    # Growth rate

time_to_80_growth(P0,K,r)