

if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dev.new()

#=====================Data Overview==========================
# Load the combined dataset
bike <- read.csv("SeoulBikeData1.csv")

# Display the structure of the dataset
str(bike)

# Print the number of observations and variables
cat("Number of observations:", nrow(bike), "\n")
cat("Number of variables:", ncol(bike), "\n")
head(bike)

#==================Summary Statistics=========================

# Calculate statistics for 'Rented_Bike_Count'
mean_Rented_Bike_Count <- mean(bike$Rented_Bike_Count, na.rm = TRUE)
median_Rented_Bike_Count <- median(bike$Rented_Bike_Count, na.rm = TRUE)
std_dev_Rented_Bike_Count <- sd(bike$Rented_Bike_Count, na.rm = TRUE)
min_Rented_Bike_Count <- min(bike$Rented_Bike_Count, na.rm = TRUE)
max_Rented_Bike_Count <- max(bike$Rented_Bike_Count, na.rm = TRUE)

# Print the results
cat("Mean:", mean_Rented_Bike_Count, "\n")
cat("Median:", median_Rented_Bike_Count, "\n")
cat("Standard Deviation:", std_dev_Rented_Bike_Count, "\n")
cat("Minimum:", min_Rented_Bike_Count, "\n")
cat("Maximum:", max_Rented_Bike_Count, "\n")

#===============Distribution Visualization====================

# Histogram for 'Rented_Bike_Count'
hist(bike$Rented_Bike_Count, 
     main = "Histogram of Rented_Bike_Count", 
     xlab = "Rented_Bike_Count", 
     col = "skyblue", 
     border = "black", 
     breaks = 20)

# Boxplot for 'residual.sugar'
boxplot(bike$Rented_Bike_Count, 
        main = "Boxplot of Rented_Bike_Count", 
        ylab = "Rented_Bike_Count", 
        col = "orange", 
        border = "brown", 
        horizontal = TRUE)

# Description of the distribution and outliers:
summary(bike$Rented_Bike_Count)
#============== Categorical Variable Analysis======================

# Bar plot for 'type'
barplot(table(bike$Holiday), 
        main = "Distribution of Holidays", 
        xlab = "Holidays", 
        ylab = "Frequency", 
        col = c("red", "green"), 
        border = "black")

# Display the counts for each wine type
table(bike$Holiday)
#=================Correlation Analysis============================

# Calculate the Pearson correlation coefficient
correlation <- cor(bike$Rented_Bike_Count,bike$Humidity_percent, use = "complete.obs", method = "pearson")

# Print the result
cat("Pearson correlation coefficient between Humidity_percent and Dew point temperature_k:", correlation, "\n")
#================Scatter plot=============================


# Scatter plot with trend line for 'fixed.acidity' and 'residual.sugar'
plot(bike$Dew.point.temperature_k, bike$Humidity_percent,
     main = "Scatter Plot of Dew.point.temperature_k vs Humidity_percent",
     xlab = "Dew.point.temperature_k",
     ylab = "Humidity_percent",
     pch = 19,               # Solid circle for points
     col = rgb(0, 0, 1, 0.5)) # Semi-transparent blue color

# Add a trend line
abline(lm(Humidity_percent ~ Dew.point.temperature_k, data = bike), col = "red", lwd = 2)

# Display the correlation coefficient
correlation <- cor(bike$Dew.point.temperature_k, bike$Humidity_percent, use = "complete.obs", method = "pearson")
cat("Pearson correlation coefficient:", correlation, "\n")

#===============linear regression model======================
# Fit a linear regression model predicting 'quality' using 'fixed.acidity' and 'residual.sugar'
model <- lm(Rented_Bike_Count ~ Dew.point.temperature_k + Humidity_percent, data = bike)

# Display the summary of the model
summary(model)
#===============Model Diagnostics============================

# Diagnostic plots for the regression model
par(mfrow = c(2, 2))  # Arrange the plots in a 2x2 grid
plot(model)

# Reset the plotting layout to default
par(mfrow = c(1, 1))
#==============PCA==========================================
# Subset numerical variables from the dataset (excluding categorical variable 'type' and 'quality')
numerical_data <- bike[, sapply(bike, is.numeric)]

# Perform PCA on the numerical data
pca_model <- prcomp(numerical_data, scale. = TRUE)

# Summary of PCA model to get the proportion of variance explained by each component
summary(pca_model)

# Plot the explained variance (Scree plot)

variances <- summary(pca_model)$importance[2,]  # Proportion of variance (row 2 of the summary)

# Plot the scree plot
plot(variances, type = "b", col = "blue", pch = 19, 
     main = "Scree Plot of PCA", 
     xlab = "Principal Components", 
     ylab = "Proportion of Variance Explained", 
     cex.main = 1.5, cex.lab = 1.2)


# Optional: Cumulative variance plot
plot(cumsum(pca_model$sdev^2) / sum(pca_model$sdev^2), 
     main = "Cumulative Explained Variance", 
     xlab = "Principal Components", 
     ylab = "Cumulative Variance Explained", 
     type = "b", col = "green")
#=================
# Biplot of the first two principal components
biplot(pca_model, main = "PCA Biplot")

# Display the loadings of the first two principal components
pca_model$rotation[, 1:2]

#==================
# Fit a polynomial regression model (degree 2) predicting 'density' 
# using 'fizxed.acidity' and 'residual.sugar'
model_poly <- lm(Rented_Bike_Count ~ poly(Dew.point.temperature_k, 3) + poly(Humidity_percent, 3), data = bike)

# Display the summary of the polynomial model
summary(model_poly)


# Diagnostic plots for the regression model
par(mfrow = c(2, 2))  # Arrange the plots in a 2x2 grid
plot(model_poly)

# Reset the plotting layout to default
par(mfrow = c(1, 1))

