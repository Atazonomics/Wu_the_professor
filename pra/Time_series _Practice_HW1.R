# Install and load the tseries package
if (!require(tseries)) {
  install.packages("tseries")
  library(tseries)
}
install.packages("magrittr")
library(magrittr) # to use cain function %.%
library(e1071)  # e1071 package provides skewness function

# Load data
data = read.csv("D:/My Drive/Atazonomics World/BSE_2023-2024/T2/Financial Econometrics/Psets/Practice/HD.csv")
head(data)

#Q1. consider the series of log returns constructed from the adusted price series
#stationarity: carry out ADF test to assess  evidecne of nonstationarity
# Log returns = ratio of adj.Closet/adj.closet-1

# Using diff(), log(), and na.omit() functions: 
# note NA values resulted from differencing

log_returns = data$Adj.Close %>%
  log() %>%
  diff(lag = 1) %>%
  na.omit()

#Augmented Dickey-Fuller Test on the log returns
adf_test_result = adf.test(log_returns)

# Test results
print(adf_test_result)

#Q2 Calcualte Moments of the return time series
# Mean
mean_value = mean(log_returns)

# Variance
variance_value = var(log_returns)

# CSkewness

skewness_value = skewness(log_returns)

# Leptokurtic (Kurtosis > 3): "heavier tails", sharper peak compared to  normal distr.
#implies a higher likelihood of extreme values (outliers), 
#means that there are more frequent large deviations from the mean than would be expected in a normal distribution.
#Mesokurtic (Kurtosis ≈ 3): means similar to normal distri interms of tail heaviness
#Platykurtic (Kurtosis < 3): light tail and less peaked 

kurtosis_value = kurtosis(log_returns)

# Results to 3 decimal places 
cat("Mean:", round(mean_value,3), "\n")
cat("Variance:", round(variance_value,3), "\n")
cat("Skewness:", round(skewness_value, 3), "\n")
cat("Kurtosis:", round(kurtosis_value,3), "\n")

#Q3: Extremes 

# Minimum return
min_return = min(log_returns)

# Maximum return
max_return = max(log_returns)

# Results
cat("Minimum Return:", min_return, "\n")
cat("Maximum Return:", max_return, "\n")


# Calculate absolute returns
absolute_returns = abs(log_returns)

# Days with absolute return larger than 5%
count_large_returns <- sum(absolute_returns > 0.05)

# Calculate the percentage
percentage_large_returns <- (count_large_returns / length(absolute_returns)) * 100

# Print the result
cat("Percentage of Days with Return > 5%:", percentage_large_returns, "%\n")

#chain pipe operator %>%
percentage_large_returns <- log_returns %>%
  abs() %>%
  `>`(0.05) %>%
  sum() %>%
  `/`(length(log_returns)) %>%
  `*`(100)

# Print the result
cat("Percentage of Days with Return > 5%:", percentage_large_returns, "%\n")

#Q4. Return Distribution. carry out 
#Jarque-Bera test ot assess evidence of normality

# JB test of normality 
jb_test_result <- jarque.bera.test(log_returns)

# Results
print(jb_test_result)

#Q5. Annualized volatility 

#caculate volatility over entire Sample 

# Calculate daily volatility (standard deviation)
daily_volatility <- sd(log_returns)

# Annualize the volatility multiply by no of working days 252
annualized_volatility <- daily_volatility * sqrt(252)

# Print the annualized volatility
cat("Annualized Volatility:", round(annualized_volatility,3), "\n")

#Calculate Annualized volatility only in 2012 
#(if stock did not trade for entire yr , enter NA)

# Ensure dates are in the proper Date format
dates <- as.Date(data$Date, format="%m/%d/%Y")

# Filter log returns for the year 2012
log_returns_2012 = log_returns[dates >= as.Date("2012-01-01") & dates <= as.Date("2012-12-31")]

# Check if data is available for the entire year
if (length(log_returns_2012) < 252) {
  annualized_volatility_2012 <- NA
} else {
  # Calculate daily volatility for 2012
  daily_volatility_2012 <- sd(log_returns_2012)
  
  # Annualize the volatility for 2012
  annualized_volatility_2012 <- daily_volatility_2012 * sqrt(252)
}

# Print the annualized volatility for 2012
cat("Annualized Volatility for 2012:", annualized_volatility_2012, "\n")


#Annualized volatility in 2015
#(if stock did not trade for entire yr , enter NA)

dates <- as.Date(data$Date, format="%m/%d/%Y")

# Filter log returns for the year 2015
log_returns_2015 = log_returns[dates >= as.Date("2015-01-01") & dates <= as.Date("2015-12-31")]

# Data available for entire year
if (length(log_returns_2015) < 252) {
  annualized_volatility_2015 <- NA
} else {
  # Calculate daily volatility for 2015
  daily_volatility_2015 <- sd(log_returns_2015)
  
  # Annualize the volatility for 2015
  annualized_volatility_2015 <- daily_volatility_2015 * sqrt(252)
}
# Print the annualized volatility for 2015
cat("Annualized Volatility for 2015:", annualized_volatility_2015, "\n")

Annualized #Annualized volatility in 2015

dates <- as.Date(data$Date, format="%m/%d/%Y")

# Filter log returns for the year 2015
log_returns_2015 = log_returns[dates >= as.Date("2015-01-01") & dates <= as.Date("2015-12-31")]

# Data available for entire year
if (length(log_returns_2015) < 252) {
  annualized_volatility_2015 <- NA
} else {
  # Calculate daily volatility for 2015
  daily_volatility_2015 <- sd(log_returns_2015)
  
  # Annualize the volatility for 2015
  annualized_volatility_2015 <- daily_volatility_2015 * sqrt(252)
}
# Print the annualized volatility for 2015
cat("Annualized Volatility for 2015:", annualized_volatility_2015, "\n")

#Annualized Volatility for 2020
#(if stock did not trade for entire yr , enter NA)

dates <- as.Date(data$Date, format="%m/%d/%Y")

# Filter log returns for the year 2020
log_returns_2020 = log_returns[dates >= as.Date("2020-01-01") & dates <= as.Date("2020-12-31")]

# Data available for entire year
if (length(log_returns_2020) < 252) {
  annualized_volatility_2020 <- NA
} else {
  # Calculate daily volatility for 2020
  daily_volatility_2020 <- sd(log_returns_2020)
  
  # Annualize the volatility for 2020
  annualized_volatility_2020 <- daily_volatility_2020 * sqrt(252)
}
# Print the annualized volatility for 2020
cat("Annualized Volatility for 2020:", annualized_volatility_2020, "\n")

#Q6. Return dynamics 

# compute sample Autocorrelations from lag 1 to 126
autocorrelations <- acf(log_returns, lag.max = 126, plot = FALSE)

# The autocorrelation values are stored in the 'acf' element of the result
autocorrelation_values <- autocorrelations$acf

# Printing the autocorrelation values
print(autocorrelation_values)

# compute sample Autocorrelations function from lag 1 to 5126
# Extract autocorrelation values for lags 1 to 5
autocorrelation_lags_1_to_5 =print( round(autocorrelations$acf[2:6],3))

# Number of autocorrelation outside the zero Confidence band ?
#If an autocorrelation coefficient falls outside this confidence band, 
#it indication observed autocorrelation is statistically significant 
#and not just a result of random chance.

# 95% Confidence level
confidence_level = 0.95

# Compute approximate confidence interval/band
# For large enough series, 
#std.E of ACF estimate is approximately 1/sqrt(N), N = no.obs
standard_error = 1 / sqrt(length(log_returns))
confidence_band <- qnorm((1 + confidence_level) / 2) * standard_error

# Determine the number of coefficients outside the confidence band
outside_band = sum(abs(autocorrelation_values) > confidence_band)

# Print the result
print(outside_band)

#Number of autocorr coefficients larger than 0.05 in absolute value
num_autocorr_greater_than_005 = sum(abs(autocorrelation_values) > 0.05)

# Print the result
cat("Number of autocorrelation coefficients > 0.05 in absolute value:", num_autocorr_greater_than_005, "\n")

#Convert Adj Close to a time series object
#stock_ts = ts(data$Adj.Close, frequency = 252)
#head(stock_ts)


#rm(list = ls()) to remove objects in the work space 
#Ctrl +L clears the console 

