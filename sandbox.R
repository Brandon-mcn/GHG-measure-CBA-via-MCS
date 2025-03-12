# Load required libraries
install.packages("sn")
library(sn)
library(ggplot2)

# Given SCC values
mean_scc <- 185   # Mean SCC (2020 USD)
std_dev_scc <- 85.7  # Estimated standard deviation
p5_scc <- 44     # 5th percentile
p95_scc <- 413   # 95th percentile

# Function to estimate skewness parameter
estimate_skewness <- function(p5, p95, mean, std_dev) {
  return ((p95 - mean) - (mean - p5)) / std_dev  # Approximate skewness
}

# Estimate skewness
skewness_param <- estimate_skewness(p5_scc, p95_scc, mean_scc, std_dev_scc)

# Generate a sequence of SCC values
x_values <- seq(10, 600, length.out = 1000)

# Compute the skew-normal density
pdf_values <- dsn(x_values, xi = mean_scc, omega = std_dev_scc, alpha = skewness_param)

# Create a dataframe for plotting
data <- data.frame(x = x_values, pdf = pdf_values)

# Plot the skew-normal distribution
ggplot(data, aes(x, pdf)) +
  geom_line(color = "purple", size = 1) +
  geom_ribbon(aes(ymin = 0, ymax = pdf), fill = "purple", alpha = 0.2) +
  geom_vline(xintercept = p5_scc, linetype = "dashed", color = "red") +
  geom_vline(xintercept = p95_scc, linetype = "dashed", color = "green") +
  labs(title = "Skew-Normal Distribution of Social Cost of Carbon (SCC)",
       x = "Social Cost of Carbon ($/tCO₂)", 
       y = "Probability Density") +
  theme_minimal()

# Print estimated skewness parameter
print(paste("Estimated Skewness Parameter:", round(skewness_param, 3)))

