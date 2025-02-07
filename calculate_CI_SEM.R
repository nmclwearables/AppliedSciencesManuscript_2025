calculate_CI_SEM <- function(data, confidence_level = 0.95) {
  
# Check if input is numeric
if (!is.numeric(data)) {
  stop("Input data must be numeric.")
}

# Remove NaN values from data
  data <- na.omit(data)
  
# If data becomes empty after removing NaNs, return an error
if (length(data) == 0) {
  stop("Data contains only NaN values after removal.")
}  
  
# Calculate sample size, mean, and standard error
n <- length(data)
mean_data <- mean(data)
std_error <- sd(data) / sqrt(n)

# Calculate the degrees of freedom
df <- n - 1

# Calculate the t-value for the given confidence level
alpha <- 1 - confidence_level
critical_value <- qt(1 - alpha / 2, df)  # Use qt for the t-distribution

# Calculate the margin of error
margin_error <- critical_value * std_error

# Calculate the confidence interval
lower_bound <- mean_data - margin_error
upper_bound <- mean_data + margin_error

# Calculate the SEM
lower_std_error <- mean_data - std_error
upper_std_error <- mean_data + std_error

# Return the confidence interval
return(c(lower_bound, upper_bound, lower_std_error, upper_std_error))

}