

if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dev.new()

#=====================Data Overview==========================
# Load the combined dataset
wine <- read.csv("wine_quality.csv")

# Display the structure of the dataset
str(wine)

# Print the number of observations and variables
cat("Number of observations:", nrow(wine), "\n")
cat("Number of variables:", ncol(wine), "\n")
head(wine)

#==================Summary Statistics=========================

# Calculate statistics for 'residual.sugar'
mean_residual_sugar <- mean(wine$residual.sugar, na.rm = TRUE)
median_residual_sugar <- median(wine$residual.sugar, na.rm = TRUE)
std_dev_residual_sugar <- sd(wine$residual.sugar, na.rm = TRUE)
min_residual_sugar <- min(wine$residual.sugar, na.rm = TRUE)
max_residual_sugar <- max(wine$residual.sugar, na.rm = TRUE)

# Print the results
cat("Mean:", mean_residual_sugar, "\n")
cat("Median:", median_residual_sugar, "\n")
cat("Standard Deviation:", std_dev_residual_sugar, "\n")
cat("Minimum:", min_residual_sugar, "\n")
cat("Maximum:", max_residual_sugar, "\n")

#===============Distribution Visualization====================

# Histogram for 'residual.sugar'
hist(wine$residual.sugar, 
     main = "Histogram of Residual Sugar", 
     xlab = "Residual Sugar", 
     col = "skyblue", 
     border = "black", 
     breaks = 20)

# Boxplot for 'residual.sugar'
boxplot(wine$residual.sugar, 
        main = "Boxplot of Residual Sugar", 
        ylab = "Residual Sugar", 
        col = "orange", 
        border = "brown", 
        horizontal = TRUE)

# Description of the distribution and outliers:
summary(wine$residual.sugar)
#============== Categorical Variable Analysis======================

# Bar plot for 'type'
barplot(table(wine$type), 
        main = "Distribution of Wine Types", 
        xlab = "Wine Type", 
        ylab = "Frequency", 
        col = c("red", "green"), 
        border = "black")

# Display the counts for each wine type
table(wine$type)
#=================Correlation Analysis============================

# Calculate the Pearson correlation coefficient
correlation <- cor(wine$fixed.acidity, wine$residual.sugar, use = "complete.obs", method = "pearson")

# Print the result
cat("Pearson correlation coefficient between fixed.acidity and residual.sugar:", correlation, "\n")
#================Scatter plot=============================


# Scatter plot with trend line for 'fixed.acidity' and 'residual.sugar'
plot(wine$fixed.acidity, wine$residual.sugar,
     main = "Scatter Plot of Fixed Acidity vs Residual Sugar",
     xlab = "Fixed Acidity",
     ylab = "Residual Sugar",
     pch = 19,               # Solid circle for points
     col = rgb(0, 0, 1, 0.5)) # Semi-transparent blue color

# Add a trend line
abline(lm(residual.sugar ~ fixed.acidity, data = wine), col = "red", lwd = 2)

# Display the correlation coefficient
correlation <- cor(wine$fixed.acidity, wine$residual.sugar, use = "complete.obs", method = "pearson")
cat("Pearson correlation coefficient:", correlation, "\n")

#===============linear regression model======================
# Fit a linear regression model predicting 'quality' using 'fixed.acidity' and 'residual.sugar'
model <- lm(density ~ fixed.acidity + residual.sugar, data = wine)

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
numerical_data <- wine[, sapply(wine, is.numeric)]

# Perform PCA on the numerical data
pca_model <- prcomp(numerical_data, scale. = TRUE)

# Summary of PCA model to get the proportion of variance explained by each component
summary(pca_model)

# Get the proportion of variance explained by each principal component
variances <- summary(pca_model)$importance[2,]  # Proportion of variance (row 2 of the summary)

# Plot the scree plot as a curve
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


