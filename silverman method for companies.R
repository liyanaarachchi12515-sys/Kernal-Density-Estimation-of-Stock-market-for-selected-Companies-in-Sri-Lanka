setwd("D:/3.1/case study 2")
getwd()

data_asian_alliance<-read.csv("ASIAN ALLIANCE.csv")
#data_asian_alliance

data_abans<-read.csv("ABANS.csv")
#data_abans

data_commercial<-read.csv("COMMERCIAL BANK.csv")
#data_commercial

data_cargills<-read.csv("CARGILLS.csv")
#data_cargills

#filling the missing data

install.packages("zoo")
library(zoo)

data_asian_alliance$High = na.approx(data_asian_alliance$High)
data_asian_alliance$Low = na.approx(data_asian_alliance$Low)
data_asian_alliance$Closing = na.approx(data_asian_alliance$Closing)
#View(data_asian_alliance)

data_abans$High = na.approx(data_abans$High)
data_abans$Low = na.approx(data_abans$Low)
data_abans$Closing = na.approx(data_abans$Closing)
#View(data_abans)

data_commercial$High = na.approx(data_commercial$High)
data_commercial$Low = na.approx(data_commercial$Low)
data_commercial$Closing = na.approx(data_commercial$Closing)
#View(data_commercial)

data_cargills$High = na.approx(data_cargills$High)
data_cargills$Low = na.approx(data_cargills$Low)
data_cargills$Closing = na.approx(data_cargills$Closing)
#View(data_cargills)

# Assuming closing_prices contains the stock closing prices
data_abans<-read.csv("ABANS.csv")
#data_abans
library(ggplot2)
# Correctly extract the numeric "Closing" column
closing_prices <- data_abans$Closing  # Replace 'Closing' with the actual column name
# Calculate standard deviation and IQR
sd_value <- sd(closing_prices)
iqr_value <- IQR(closing_prices)

# Calculate the number of data points
n <- length(closing_prices)

# Silverman's rule of thumb for bandwidth
silverman_h <- 0.9 * min(sd_value, iqr_value / 1.34) * n^(-1/5)
silverman_h
# Define adjusted bandwidths
h_reduced <- silverman_h - 5
h_increased <- silverman_h + 5

# Ensure that bandwidths are positive
h_reduced <- max(h_reduced, 0.1)  # A minimal bandwidth value to avoid issues
h_increased <- max(h_increased, 0.1)

# Print the bandwidth values
cat("Bandwidth values:\n")
cat("Silverman's Rule of Thumb (h):", silverman_h, "\n")
cat("Reduced Bandwidth (h - 5):", h_reduced, "\n")
cat("Increased Bandwidth (h + 5):", h_increased, "\n")

# Calculate density estimates for each bandwidth
density_silverman <- density(closing_prices, bw = silverman_h)
density_reduced <- density(closing_prices, bw = h_reduced)
density_increased <- density(closing_prices, bw = h_increased)

# Create data frames for plotting
density_data_silverman <- data.frame(x = density_silverman$x, y = density_silverman$y, Bandwidth = "Silverman's")
density_data_reduced <- data.frame(x = density_reduced$x, y = density_reduced$y, Bandwidth = "Reduced (-5)")
density_data_increased <- data.frame(x = density_increased$x, y = density_increased$y, Bandwidth = "Increased (+5)")

# Combine all data
all_density_data <- rbind(density_data_silverman, density_data_reduced, density_data_increased)

# Plot all densities in one graph
ggplot(all_density_data, aes(x = x, y = y, color = Bandwidth)) +
  geom_line(size = 1) +
  labs(title = "Kernel Density Estimate with Different Bandwidths Abans",
       x = "Closing Prices",
       y = "Density",
       color = "Bandwidth") +
  theme_minimal()




# Assuming closing_prices contains the stock closing prices
data_commercial<-read.csv("COMMERCIAL BANK.csv")
#data_commercial
library(ggplot2)
# Correctly extract the numeric "Closing" column
closing_prices <- data_commercial$Closing  # Replace 'Closing' with the actual column name
# Calculate standard deviation and IQR
sd_value <- sd(closing_prices)
iqr_value <- IQR(closing_prices)

# Calculate the number of data points
n <- length(closing_prices)

# Silverman's rule of thumb for bandwidth
silverman_h <- 0.9 * min(sd_value, iqr_value / 1.34) * n^(-1/5)

# Define adjusted bandwidths
h_reduced <- silverman_h - 5
h_increased <- silverman_h + 5

# Ensure that bandwidths are positive
h_reduced <- max(h_reduced, 0.1)  # A minimal bandwidth value to avoid issues
h_increased <- max(h_increased, 0.1)

# Print the bandwidth values
cat("Bandwidth values:\n")
cat("Silverman's Rule of Thumb (h):", silverman_h, "\n")
cat("Reduced Bandwidth (h - 5):", h_reduced, "\n")
cat("Increased Bandwidth (h + 5):", h_increased, "\n")

# Calculate density estimates for each bandwidth
density_silverman <- density(closing_prices, bw = silverman_h)
density_reduced <- density(closing_prices, bw = h_reduced)
density_increased <- density(closing_prices, bw = h_increased)

# Create data frames for plotting
density_data_silverman <- data.frame(x = density_silverman$x, y = density_silverman$y, Bandwidth = "Silverman's")
density_data_reduced <- data.frame(x = density_reduced$x, y = density_reduced$y, Bandwidth = "Reduced (-5)")
density_data_increased <- data.frame(x = density_increased$x, y = density_increased$y, Bandwidth = "Increased (+5)")

# Combine all data
all_density_data <- rbind(density_data_silverman, density_data_reduced, density_data_increased)

# Plot all densities in one graph
ggplot(all_density_data, aes(x = x, y = y, color = Bandwidth)) +
  geom_line(size = 1) +
  labs(title = "Kernel Density Estimate with Different Bandwidths Commercial",
       x = "Closing Prices",
       y = "Density",
       color = "Bandwidth") +
  theme_minimal()




# Assuming closing_prices contains the stock closing prices
data_cargills<-read.csv("CARGILLS.csv")



library(ggplot2)
# Correctly extract the numeric "Closing" column
closing_prices <- data_cargills$Closing  # Replace 'Closing' with the actual column name
# Calculate standard deviation and IQR
sd_value <- sd(closing_prices)
iqr_value <- IQR(closing_prices)

# Calculate the number of data points
n <- length(closing_prices)

# Silverman's rule of thumb for bandwidth
silverman_h <- 0.9 * min(sd_value, iqr_value / 1.34) * n^(-1/5)
silverman_h
# Define adjusted bandwidths
h_reduced <- silverman_h - 2
h_increased <- silverman_h + 2

# Ensure that bandwidths are positive
h_reduced <- max(h_reduced, 0.1)  # A minimal bandwidth value to avoid issues
h_increased <- max(h_increased, 0.1)

# Print the bandwidth values
cat("Bandwidth values:\n")
cat("Silverman's Rule of Thumb (h):", silverman_h, "\n")
cat("Reduced Bandwidth (h - 2):", h_reduced, "\n")
cat("Increased Bandwidth (h + 2):", h_increased, "\n")

# Calculate density estimates for each bandwidth
density_silverman <- density(closing_prices, bw = silverman_h)
density_reduced <- density(closing_prices, bw = h_reduced)
density_increased <- density(closing_prices, bw = h_increased)

# Create data frames for plotting
density_data_silverman <- data.frame(x = density_silverman$x, y = density_silverman$y, Bandwidth = "Silverman's")
density_data_reduced <- data.frame(x = density_reduced$x, y = density_reduced$y, Bandwidth = "Reduced (-2)")
density_data_increased <- data.frame(x = density_increased$x, y = density_increased$y, Bandwidth = "Increased (+2)")

# Combine all data
all_density_data <- rbind(density_data_silverman, density_data_reduced, density_data_increased)

# Plot all densities in one graph
ggplot(all_density_data, aes(x = x, y = y, color = Bandwidth)) +
  geom_line(size = 1) +
  labs(title = "Kernel Density Estimate with Different Bandwidths cargills",
       x = "Closing Prices",
       y = "Density",
       color = "Bandwidth") +
  theme_minimal()
h_reduced 
h_increased
silverman_h
###########
library(ggplot2)

# Correctly extract the numeric "Closing" column
closing_prices <- data_asian_alliance$Closing  # Replace 'Closing' with the actual column name

# Check for missing values
cat("Number of missing values in 'closing_prices':", sum(is.na(closing_prices)), "\n")

# Remove missing values
closing_prices <- na.omit(closing_prices)

# Calculate standard deviation and IQR
sd_value <- sd(closing_prices)            # Standard deviation
iqr_value <- IQR(closing_prices)          # Interquartile range

# Print standard deviation and IQR
cat("Standard Deviation (sd):", sd_value, "\n")
cat("Interquartile Range (IQR):", iqr_value, "\n")

# Calculate the number of data points
n <- length(closing_prices)
cat("Number of valid data points:", n, "\n")

# Silverman's rule of thumb for bandwidth
silverman_h <- 0.9 * min(sd_value, iqr_value / 1.34) * n^(-1/5)
cat("Bandwidth using Silverman's Rule:", silverman_h, "\n")

# Define adjusted bandwidths
h_reduced <- max(silverman_h - 5, 0.1)  # Ensure bandwidth is positive
h_increased <- silverman_h + 5

# Print the bandwidth values
cat("Reduced Bandwidth (h - 5):", h_reduced, "\n")
cat("Increased Bandwidth (h + 5):", h_increased, "\n")

# Calculate density estimates for each bandwidth
density_silverman <- density(closing_prices, bw = silverman_h)
density_reduced <- density(closing_prices, bw = h_reduced)
density_increased <- density(closing_prices, bw = h_increased)

# Create data frames for plotting
density_data_silverman <- data.frame(x = density_silverman$x, y = density_silverman$y, Bandwidth = "Silverman's")
density_data_reduced <- data.frame(x = density_reduced$x, y = density_reduced$y, Bandwidth = "Reduced (-5)")
density_data_increased <- data.frame(x = density_increased$x, y = density_increased$y, Bandwidth = "Increased (+5)")

# Combine all data
all_density_data <- rbind(density_data_silverman, density_data_reduced, density_data_increased)

# Plot all densities in one graph
ggplot(all_density_data, aes(x = x, y = y, color = Bandwidth)) +
  geom_line(size = 1) +
  labs(title = "Kernel Density Estimate with Different Bandwidths asian alliance",
       x = "Closing Prices",
       y = "Density",
       color = "Bandwidth") +
  theme_minimal()