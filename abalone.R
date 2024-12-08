if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


dev.new()

# Reading the data
my_data <- read.csv("abalone_dataset.csv")
str(my_data)
head(my_data)

# Number of observations and variables
dim(my_data)
# Number of rows (observations)
nrow(my_data)

# Number of columns (variables)
ncol(my_data)

# Calculate summary statistics for 'Diameter'
mean_Dia <- mean(my_data$Diameter, na.rm = TRUE)
median_Dia <- median(my_data$Diameter, na.rm = TRUE)
sd_Dia <- sd(my_data$Diameter, na.rm = TRUE)
min_Dia <- min(my_data$Diameter, na.rm = TRUE)
max_Dia <- max(my_data$Diameter, na.rm = TRUE)

# Create a data frame to store the results
summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Minimum", "Maximum"),
  Value = c(mean_Dia, median_Dia, sd_Dia, min_Dia, max_Dia)
)

# Print the table
print(summary_table)


# Select numerical variables
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Calculate statistics
stats <- data.frame(
  Variable = colnames(numerical_vars),
  Min = sapply(numerical_vars, min),
  Max = sapply(numerical_vars, max),
  Mean = sapply(numerical_vars, mean),
  Median = sapply(numerical_vars, median),
  StdDev = sapply(numerical_vars, sd)
)

# Display the statistics table
library(knitr)  # For better table formatting
kable(stats, caption = "Summary Statistics for Numerical Variables")


# Create a histogram
hist(my_data$Diameter,
     main = "Histogram of Diameter",
     xlab = "Diameter",
     col = "skyblue",
     border = "black")

# Create a boxplot
boxplot(my_data$Diameter,
        main = "Boxplot of Diameter",
        ylab = "Diameter",
        col = "lightgreen",
        horizontal = TRUE)

# Create a bar plot for the 'Sex' variable
barplot(table(my_data$Sex),
        main = "Distribution of Sex",
        xlab = "Sex",
        ylab = "Frequency",
        col = c("skyblue", "pink"),
        border = "black")

# Multivariate Analysis
# ================ Pearson Correlation ==================
# Calculate Pearson correlation between 'Height' and 'Viscera.weight'
correlation <- cor(my_data$Height, my_data$Viscera.weight, method = "pearson", use = "complete.obs")

# Print the result
cat("Pearson Correlation Coefficient between Height and Viscera.weight:", correlation)


# Extract numerical variables
numerical_vars <- my_data[, sapply(my_data, is.numeric)]

# Calculate the correlation matrix for numerical variables
correlation_matrix <- cor(numerical_vars)

# Print the correlation matrix
print(correlation_matrix)



# ================ scatter plot ==================

# Create a scatter plot
# Create a scatter plot
plot(my_data$Height, my_data$Viscera.weight,
     main = "Scatter Plot of Height vs Viscera.weight",
     xlab = "Height",
     ylab = "Viscera.weight",
     col = "blue",    # Color of the points
     pch = 16)        # Shape of the points

# Add a regression line (trend line)
abline(lm(Viscera.weight ~ Height, data = my_data), col = "red")  # Red trend line


# ================ linear regression model ==================

# Fit a linear regression model predicting 'Rings' from 'Diameter' and 'Whole.weight'
model <- lm(Rings ~ Height + Viscera.weight, data = my_data)

# Display the summary of the model
summary(model)

# ================ Use all the variables ==================

# Fit a linear regression model predicting Performance.Index from Previous.Scores and Hours.Studied
# Fit a linear regression model using all variables to predict Rings
model <- lm(Rings ~ Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, data = my_data)

# Display the summary of the model
summary(model)

# Diagnostic plots for the updated model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)

# =============== Poly fit ========================


# Fit a polynomial regression model (second-degree) using the predictors
model_poly <- lm(Rings ~ poly(Diameter, 2) + poly(Height, 2) + poly(Whole.weight, 2) + 
                   poly(Shucked.weight, 2) + poly(Viscera.weight, 2) + poly(Shell.weight, 2), data = my_data)

# Display the summary of the polynomial model
summary(model_poly)

# Diagnostic plots for the polynomial model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model_poly)

# =============== heteroscedasticity applying log transformation ==================

# Apply log transformation to the dependent variable
my_data$log_Rings <- log(my_data$Rings)

# Fit a polynomial regression model (second-degree) using the predictors with log-transformed Rings
model_poly_log <- lm(log_Rings ~ poly(Diameter, 2) + poly(Height, 2) + poly(Whole.weight, 2) + 
                       poly(Shucked.weight, 2) + poly(Viscera.weight, 2) + poly(Shell.weight, 2), data = my_data)

# Display the summary of the log-transformed polynomial model
summary(model_poly_log)

# Diagnostic plots for the log-transformed polynomial model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model_poly_log)




# ================ Principal Component Analysis (PCA) ====================

# Step 1: Select numerical variables from the dataset
numerical_vars <- my_data[, c("Length", "Diameter", "Height", "Whole.weight", 
                              "Shucked.weight", "Viscera.weight", "Shell.weight")]

# Step 2: Standardize the data
numerical_vars_scaled <- scale(numerical_vars)

# Step 3: Perform PCA
pca_result <- prcomp(numerical_vars_scaled, center = TRUE, scale. = TRUE)

# Step 4: Plot the explained variance (Scree plot)
# Variance explained by each component
explained_variance <- summary(pca_result)$importance[2,]

# Plot the explained variance
plot(explained_variance, type = "b", main = "Scree Plot", xlab = "Principal Components", 
     ylab = "Proportion of Variance Explained", pch = 19, col = "blue", lwd = 2)

# Optional: Add a line for cumulative variance to help with choosing components
cumulative_variance <- cumsum(explained_variance)


# ================ Visualize the PCA results ====================
biplot(pca_result, scale = 0, main = "PCA Biplot (Scaled)", col = c("black", "red"))



















