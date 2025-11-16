
setwd("D:/3.1/case study 2")
getwd()

data_abans<-read.csv("ABANS.csv")
#data_abans



# Load necessary library
library(zoo)  # for na.approx()

# Step 1: Handle missing data (impute using na.approx)

data_abans$High = na.approx(data_abans$High)
data_abans$Low = na.approx(data_abans$Low)
data_abans$Closing = na.approx(data_abans$Closing)
#View(data_abans)
fivenum(data_abans$Closing)
# Step 2: Define the Epanechnikov Kernel
custom_kernel <- function(u) {
  ifelse(abs(u) <= 1, (3 / 4) * (1 - u^2), 0)
}

# Step 3: Define Kernel Density Estimate Function
kde <- function(x, data, h, kernel_func) {
  n <- length(data)
  density <- sapply(x, function(xi) {
    sum(kernel_func((xi - data) / h)) / (n * h)
  })
  return(density)
}

# Step 4: Cross-Validation Function for CV(h)
cv <- function(h, data, kernel_func) {
  n <- length(data)
  
  # Term 1: Integral approximation (numerical integration of squared KDE)
  x_vals <- seq(min(data) - 2 * h, max(data) + 2 * h, length.out = 1000)  # Fine grid for integration
  f_h <- kde(x_vals, data, h, kernel_func)
  term1 <- sum(f_h^2) * (x_vals[2] - x_vals[1])  # Trapezoidal integration
  
  # Term 2: Leave-one-out density estimates
  term2 <- 0
  for (i in 1:n) {
    f_minus_i <- kde(data[i], data[-i], h, kernel_func)
    term2 <- term2 + f_minus_i
  }
  term2 <- (2 / n) * term2
  
  # CV(h) = Term 1 - Term 2
  cv_value <- term1 - term2
  return(cv_value)
}

# Step 5: Example Dataset (Ensure the data is numeric)
data <- data_abans$Closing # Using High as the example data

# Calculate a reasonable range for h based on the data
#h_min <- diff(range(data)) / 50  # Minimum bandwidth as a fraction of data range
#h_max <- diff(range(data)) / 2   # Maximum bandwidth as a fraction of data range

# Generate bandwidth values between h_min and h_max
#h_values <- seq(h_min, h_max, length.out = 100)  # Bandwidth range to test
# Step 6: Define Bandwidth Grid
h_values <- seq(9.5,100 , by = 0.1)  # Bandwidth range to test

# Step 7: Compute CV(h) for Each Bandwidth
cv_values <- sapply(h_values, function(h) cv(h, data, custom_kernel))

# Step 8: Find Optimal Bandwidth
optimal_h <- h_values[which.min(cv_values)]

# Step 9: Display Results
cat("Optimal bandwidth (h):", optimal_h, "\n")
cat("CV values:\n")
print(data.frame(h = h_values, CV = cv_values))

# Step 10: Plot CV(h)
plot(h_values, cv_values, type = "b", col = "blue",
     xlab = "Bandwidth (h)", ylab = "CV(h)", main = "Cross-Validation for Bandwidth Selection Abans")
points(optimal_h, min(cv_values), col = "red", pch = 19)  # Highlight minimum



h <-optimal_h
h





# Step 11: Generate KDE Using Optimal Bandwidth
x_vals <- seq(min(data) - 2 * h, max(data) + 2 * h, length.out = 1000)  # Fine grid for KDE plot
kde_values <- kde(x_vals, data, h, custom_kernel)

# Step 12: Plot KDE
plot(x_vals, kde_values, type = "l", col = "blue", lwd = 2,
     xlab = "Data (close Prices)", ylab = "Density",
     main = "Kernel Density Estimate (KDE) with Optimal Bandwidth Abans")

}