library(brms)
require(brms)
library(loo)
require(loo)
library(dplyr)
require(dplyr)
library(bridgesampling)
require(bridgesampling)
library(caret)
library(ggplot2)
library(coda)
library(rstan)
require(rstan)
library(bayesplot)

carprice = read.csv("D:/Study folder/Advanced bayesian Data Analytics/CarPrice_Assignment.csv")
carprice
#carprice$CarName = factor(carprice$CarName, levels = c("alfa_romero","audi","bmw", "buick", "chevrolet","dodge","honda","isuzu","jaguar", "maxda","mazda","mercury","mitsubishi","nissan","peugeot","plymouth","porsche","renault","saab","subaru","toyota","volkswagen","volvo"), labels(1,0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,222,23))



# Extract only the brand names from the "carName" column
carprice$carname <- sub("\\s+.*", "", carprice$CarName)

# Fix misspelled brand names
carprice$carname <- gsub("vw", "volkswagen", carprice$carname)
carprice$carname <- gsub("vokswagen", "volkswagen", carprice$carname)
carprice$carname <- gsub("toyouta", "toyota", carprice$carname)
carprice$carname <- gsub("maxda", "mazda", carprice$carname)
carprice$carname <- gsub("porcshce", "porsche", carprice$carname)

print(carprice)


# Separate numerical and categorical columns
numerical_columns <- sapply(carprice, is.numeric)
categorical_columns <- setdiff(names(carprice), names(carprice)[numerical_columns])

# Standardize numerical columns
carprice[, numerical_columns] <- scale(carprice[, numerical_columns])

# One-hot encode categorical columns
encoded_data <- predict(dummyVars(~ ., data = carprice[, categorical_columns]), newdata = carprice)

# Combine the standardized numerical columns with the encoded categorical columns
final_data <- cbind(carprice[, numerical_columns, drop = FALSE], encoded_data)
final_data

#Apply Bayesian GLM
#MOdel 1
carprice_regr = brm(price~wheelbase + carnamebmw + fueltypegas + carlength + carwidth + carheight + curbweight + enginesize + horsepower + peakrpm, data=final_data, family = gaussian(),save_pars = save_pars(all = TRUE), set_prior("normal(0,3)",class = "b")) 
summary(carprice_regr)


#model 2
carprice_regr2 = brm(price~wheelbase + carnameaudi + fueltypegas + carlength + carwidth + carheight + curbweight + enginesize + horsepower + peakrpm, data=final_data,family = gaussian(), save_pars = save_pars(all = TRUE), set_prior("normal(0,3)",class = "b")) 
summary(carprice_regr2)

#model 3
carprice_regr3 = brm(price~wheelbase + carnametoyota + fueltypegas + carlength + carwidth + carheight + curbweight + enginesize + horsepower + peakrpm, data=final_data,family = gaussian(),save_pars = save_pars(all = TRUE), set_prior("normal(0,3)",class = "b")) 
summary(carprice_regr3)




# Run convergence diagnostics
carprice_diagnosis <- brm(
  formula = price ~ wheelbase + carnamebmw + fueltypegas + carlength + carwidth + carheight + curbweight + enginesize + horsepower + peakrpm,
  data = final_data,
  family = gaussian(),
  iter = 2000,  # Total number of iterations
  warmup = 1000,  # Number of warmup iterations
  cores = 4  # Number of CPU cores to use
)


# Print a summary of the convergence diagnostics
print(carprice_diagnosis)

# Plot the convergence diagnostics
plot(carprice_diagnosis)

mcmc_trace(carprice_diagnosis)




# Run convergence diagnostics2
carprice_diagnosis2 <- brm(
  formula = price ~ wheelbase + carnameaudi + fueltypegas + carlength + carwidth + carheight + curbweight + enginesize + horsepower + peakrpm,
  data = final_data,
  family = gaussian(),
  iter = 2000,  # Total number of iterations
  warmup = 1000,  # Number of warmup iterations
  cores = 4  # Number of CPU cores to use
)


# Print a summary of the convergence diagnostics
print(carprice_diagnosis2)

# Plot the convergence diagnostics
plot(carprice_diagnosis2)

mcmc_trace(carprice_diagnosis2)



# Run convergence diagnostics3
carprice_diagnosis3 <- brm(
  formula = price ~ wheelbase + carnametoyota + fueltypegas + carlength + carwidth + carheight + curbweight + enginesize + horsepower + peakrpm,
  data = final_data,
  family = gaussian(),
  iter = 2000,  # Total number of iterations
  warmup = 1000,  # Number of warmup iterations
  cores = 4  # Number of CPU cores to use
)


# Print a summary of the convergence diagnostics
print(carprice_diagnosis3)

# Plot the convergence diagnostics
plot(carprice_diagnosis3)

mcmc_trace(carprice_diagnosis3)





# Compare models

bayes_factor(carprice_regr, carprice_regr2)
bayes_factor(carprice_regr, carprice_regr3)
bayes_factor(carprice_regr2, carprice_regr3)

#  carprice_regr, carprice_regr2, and carprice_regr3 model comparision
waic(carprice_regr)
waic(carprice_regr2)
waic(carprice_regr3)




# coefficient plot
plot(carprice_regr)
plot(carprice_regr2)
plot(carprice_regr3)

# posterior predictive plot
pp_check(carprice_regr)
pp_check(carprice_regr2)
pp_check(carprice_regr3)

# Posterior Prediction
pp <- posterior_predict(carprice_regr)
pp
pp2 <- posterior_predict(carprice_regr2)
pp2
pp3 <- posterior_predict(carprice_regr3)
pp3

#REsIdual plot
model_fitted <- fitted(carprice_regr)
model_residuals <- residuals(carprice_regr)

# Create a residual plot
plot(model_fitted, model_residuals, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lty = 2, col = "red")  # Add a dashed line at y = 0 for reference

#REsIdual plot2
model_fitted2 <- fitted(carprice_regr2)
model_residuals2 <- residuals(carprice_regr2)

# Create a residual plot
plot(model_fitted2, model_residuals2, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lty = 2, col = "red")  # Add a dashed line at y = 0 for reference


#REsIdual plot3
model_fitted3 <- fitted(carprice_regr3)
model_residuals3 <- residuals(carprice_regr3)

# Create a residual plot
plot(model_fitted3, model_residuals3, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lty = 2, col = "red")  # Add a dashed line at y = 0 for reference


dim(predicted_prices)
length(mean_predicted_prices)  # Should be 205
length(actual_prices)  # Should be 205

mean_predicted_prices <- colMeans(predicted_prices)  # Takes mean for each observation


length(mean_predicted_prices)  # Should be 205
length(actual_prices)  # Should be 205


results <- data.frame(actual_prices, mean_predicted_prices)
write.csv(results, "predictions.csv", row.names = FALSE)

getwd()


#Calculating preidiction accuracy




install.packages("Metrics")

library(Metrics)

# Read data
data <- read.csv("predictions.csv")

# Extract actual and predicted values
actual_prices <- data$actual_prices
predicted_prices <- data$mean_predicted_prices

# Calculate MAPE manually
mape_value <- mean(abs((actual_prices - predicted_prices) / actual_prices)) * 100

# Compute accuracy
accuracy <- 100 - mape_value

# Print accuracy
print(paste("Prediction Accuracy:", round(accuracy, 2), "%"))

str(data)  # Check structure of the dataset
summary(data)  # Get basic statistics

head(actual_prices)
head(predicted_prices)

actual_prices <- exp(actual_prices)
predicted_prices <- exp(predicted_prices)


error_values <- abs(actual_prices - predicted_prices)
summary(error_values)  # Look for very large errors


mape_value <- mean(abs((actual_prices - predicted_prices) / actual_prices)) * 100
accuracy <- 100 - mape_value
print(paste("Updated Prediction Accuracy:", round(accuracy, 2), "%"))


