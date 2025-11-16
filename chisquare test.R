setwd("D:/3.1/case study 2")
getwd()

data_asian_alliance<-read.csv("ASIAN ALLIANCE.csv")



# Load necessary library
library(zoo)  # for na.approx()

# Step 1: Handle missing data (impute using na.approx)
data_asian_alliance$High = na.approx(data_asian_alliance$High)
data_asian_alliance$Low = na.approx(data_asian_alliance$Low)
data_asian_alliance$Closing = na.approx(data_asian_alliance$Closing)
data_asian_alliance
# Step 2: Set the optimal bandwidth
optimal_h <- 15.7

# Step 3: Perform Kernel Density Estimation
kde <- density(data_asian_alliance$Closing, bw = optimal_h)
close_prices<-data_asian_alliance$Closing
# Step 4: Create bins
num_bins <- 10
bin_edges <- seq(min(close_prices), max(close_prices), length.out = num_bins + 1)
bin_centers <- (bin_edges[-1] + bin_edges[-length(bin_edges)]) / 2

# Step 5: Calculate observed frequencies
observed_freq <- hist(close_prices, breaks = bin_edges, plot = FALSE)$counts

# Step 6: Calculate theoretical frequencies
bin_width <- bin_edges[2] - bin_edges[1]
theoretical_probs <- approx(kde$x, kde$y, xout = bin_centers)$y * bin_width
theoretical_freq <- theoretical_probs * length(close_prices)

# Step 7: Compute Chi-Square Statistic
chi_square_stat <- sum((observed_freq - theoretical_freq)^2 / theoretical_freq)

# Step 8: Degrees of freedom and critical value

degrees_of_freedom <- num_bins - 1
critical_value <- qchisq(0.95, degrees_of_freedom)

# Step 9: Compare and Output
cat("Chi-Square Statistic:", chi_square_stat, "\n")
cat("Critical Value (95% confidence):", critical_value, "\n")
cat("Does the data fit the model? ", chi_square_stat < critical_value, "\n")











#################commercial############################
data_commercial<-read.csv("COMMERCIAL BANK.csv")


# Load necessary library
library(zoo)  # for na.approx()
data_commercial$High = na.approx(data_commercial$High)
data_commercial$Low = na.approx(data_commercial$Low)
data_commercial$Closing = na.approx(data_commercial$Closing)


# Step 2: Set the optimal bandwidth
optimal_h <- 6.018

# Step 1: Load the data
close_prices <-data_commercial$Closing # Replace with your column name if different



# Step 3: Perform Kernel Density Estimation
kde <- density(close_prices, bw = optimal_h)

# Step 4: Create bins
num_bins <- 10  # Adjust as necessary
bin_edges <- seq(min(close_prices), max(close_prices), length.out = num_bins + 1)
bin_centers <- (bin_edges[-1] + bin_edges[-length(bin_edges)]) / 2

# Step 5: Calculate observed frequencies
observed_freq <- hist(close_prices, breaks = bin_edges, plot = FALSE)$counts

# Step 6: Calculate theoretical frequencies
bin_width <- bin_edges[2] - bin_edges[1]

# Restrict bin centers to the range of kde$x to avoid NA from `approx`
bin_centers <- bin_centers[bin_centers >= min(kde$x) & bin_centers <= max(kde$x)]

# Interpolate KDE values at bin centers
theoretical_probs <- approx(kde$x, kde$y, xout = bin_centers)$y * bin_width

# Handle zero or negative values in theoretical probabilities
theoretical_probs[theoretical_probs <= 0] <- 1e-10

# Convert probabilities to theoretical frequencies
theoretical_freq <- theoretical_probs * length(close_prices)

# Step 7: Compute Chi-Square Statistic
chi_square_stat <- sum((observed_freq - theoretical_freq)^2 / theoretical_freq)

# Step 8: Degrees of freedom and critical value

degrees_of_freedom <- num_bins - 1
critical_value <- qchisq(0.95, degrees_of_freedom)

# Step 9: Compare and Output
cat("Chi-Square Statistic:", chi_square_stat, "\n")
cat("Critical Value (95% confidence):", critical_value, "\n")
cat("Does the data fit the model? ", chi_square_stat < critical_value, "\n")

##################Abans###############
data_abans<-read.csv("ABANS.csv")
data_abans



# Load necessary library
library(zoo)  # for na.approx()

# Step 1: Handle missing data (impute using na.approx)

data_abans$High = na.approx(data_abans$High)
data_abans$Low = na.approx(data_abans$Low)
data_abans$Closing = na.approx(data_abans$Closing)


# Step 2: Set the optimal bandwidth
optimal_h <- 12.4

# Step 1: Load the data
close_prices <-data_abans$Closing # Replace with your column name if different



# Step 3: Perform Kernel Density Estimation
kde <- density(close_prices, bw = optimal_h)

# Step 4: Create bins
num_bins <- 10  # Adjust as necessary
bin_edges <- seq(min(close_prices), max(close_prices), length.out = num_bins + 1)
bin_centers <- (bin_edges[-1] + bin_edges[-length(bin_edges)]) / 2

# Step 5: Calculate observed frequencies
observed_freq <- hist(close_prices, breaks = bin_edges, plot = FALSE)$counts

# Step 6: Calculate theoretical frequencies
bin_width <- bin_edges[2] - bin_edges[1]

# Restrict bin centers to the range of kde$x to avoid NA from `approx`
bin_centers <- bin_centers[bin_centers >= min(kde$x) & bin_centers <= max(kde$x)]

# Interpolate KDE values at bin centers
theoretical_probs <- approx(kde$x, kde$y, xout = bin_centers)$y * bin_width

# Handle zero or negative values in theoretical probabilities
theoretical_probs[theoretical_probs <= 0] <- 1e-10

# Convert probabilities to theoretical frequencies
theoretical_freq <- theoretical_probs * length(close_prices)

# Step 7: Compute Chi-Square Statistic
chi_square_stat <- sum((observed_freq - theoretical_freq)^2 / theoretical_freq)

# Step 8: Degrees of freedom and critical value

degrees_of_freedom <- num_bins - 1
critical_value <- qchisq(0.95, degrees_of_freedom)

# Step 9: Compare and Output
cat("Chi-Square Statistic:", chi_square_stat, "\n")
cat("Critical Value (95% confidence):", critical_value, "\n")
cat("Does the data fit the model? ", chi_square_stat < critical_value, "\n")




#########cargills##############
data_cargills<-read.csv("CARGILLS.csv")




# Load necessary library
library(zoo)  # for na.approx()


# Step 1: Handle missing data (impute using na.approx)

data_cargills$High = na.approx(data_cargills$High)
data_cargills$Low = na.approx(data_cargills$Low)
data_cargills$Closing = na.approx(data_cargills$Closing)

# Step 2: Set the optimal bandwidth
optimal_h <- 6.3

# Step 1: Load the data
close_prices <-data_abans$Closing # Replace with your column name if different



# Step 3: Perform Kernel Density Estimation
kde <- density(close_prices, bw = optimal_h)

# Step 4: Create bins
num_bins <- 10  # Adjust as necessary
bin_edges <- seq(min(close_prices), max(close_prices), length.out = num_bins + 1)
bin_centers <- (bin_edges[-1] + bin_edges[-length(bin_edges)]) / 2

# Step 5: Calculate observed frequencies
observed_freq <- hist(close_prices, breaks = bin_edges, plot = FALSE)$counts

# Step 6: Calculate theoretical frequencies
bin_width <- bin_edges[2] - bin_edges[1]

# Restrict bin centers to the range of kde$x to avoid NA from `approx`
bin_centers <- bin_centers[bin_centers >= min(kde$x) & bin_centers <= max(kde$x)]

# Interpolate KDE values at bin centers
theoretical_probs <- approx(kde$x, kde$y, xout = bin_centers)$y * bin_width

# Handle zero or negative values in theoretical probabilities
theoretical_probs[theoretical_probs <= 0] <- 1e-10

# Convert probabilities to theoretical frequencies
theoretical_freq <- theoretical_probs * length(close_prices)

# Step 7: Compute Chi-Square Statistic
chi_square_stat <- sum((observed_freq - theoretical_freq)^2 / theoretical_freq)

# Step 8: Degrees of freedom and critical value
degrees_of_freedom <- num_bins - 1
critical_value <- qchisq(0.95, degrees_of_freedom)

# Step 9: Compare and Output
cat("Chi-Square Statistic:", chi_square_stat, "\n")
cat("Critical Value (95% confidence):", critical_value, "\n")
cat("Does the data fit the model? ", chi_square_stat < critical_value, "\n")


