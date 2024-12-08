
if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dev.new()

# Load the dataset
students_data <- read.csv("StudentsPerformance.csv")
str(students_data)
# ===================== Single Variable Analysis for Math Score =====================

# Summary Statistics for Math Score
mean_math_score <- mean(students_data$math.score, na.rm = TRUE)
median_math_score <- median(students_data$math.score, na.rm = TRUE)
std_dev_math_score <- sd(students_data$math.score, na.rm = TRUE)
min_math_score <- min(students_data$math.score, na.rm = TRUE)
max_math_score <- max(students_data$math.score, na.rm = TRUE)

cat("Mean Math Score:", mean_math_score, "\n")
cat("Median Math Score:", median_math_score, "\n")
cat("Standard Deviation Math Score:", std_dev_math_score, "\n")
cat("Min Math Score:", min_math_score, "\n")
cat("Max Math Score:", max_math_score, "\n")

# Distribution Visualization for Math Score
hist(students_data$math.score, main = "Histogram of Math Score", xlab = "Math Score", col = "skyblue", border = "black", breaks = 10)
boxplot(students_data$math.score, main = "Boxplot of Math Score", ylab = "Math Score", col = "orange", border = "brown", horizontal = TRUE)

# ===================== Categorical Variable Analysis for Gender =====================

# Frequency count for 'gender'
cat("Frequency distribution of Gender (Sex):\n")
print(table(students_data$gender))

# Bar plot for gender
barplot(table(students_data$gender), main = "Distribution of Gender", 
        xlab = "Gender", ylab = "Frequency", 
        col = c("lightblue", "lightpink"), border = "black")

# ===================== Linear Regression Model =====================
# Load the dataset
#students_data <- read.csv("/Users/bujji/Documents/ISI/SSD/Assignment/Data4/StudentsPerformance.csv")

# Scatter plot with trend line for 'math.score' and 'reading.score'
plot(students_data$math.score, students_data$reading.score,
     main = "Scatter Plot of Math Score vs Reading Score",
     xlab = "Math Score",
     ylab = "Reading Score",
     pch = 19,               # Solid circle for points
     col = rgb(0, 0, 1, 0.5)) # Semi-transparent blue color

# Add a trend line
abline(lm(reading.score ~ math.score, data = students_data), col = "red", lwd = 2)

# Display the correlation coefficient
correlation <- cor(students_data$math.score, students_data$reading.score, use = "complete.obs", method = "pearson")
cat("Pearson correlation coefficient:", correlation, "\n")




# Fit a linear regression model predicting 'writing.score' using 'math.score' and 'reading.score'
model <- lm(writing.score ~ math.score + reading.score, data = students_data)

# Display the summary of the model
summary(model)

# ===================== Model Diagnostics =====================

# Diagnostic plots for the regression model
par(mfrow = c(2, 2))  # Arrange the plots in a 2x2 grid
plot(model)

# Reset the plotting layout to default
par(mfrow = c(1, 1))

# ===================== PCA (Principal Component Analysis) =====================

# Subset numerical variables for PCA (excluding categorical variables)
numerical_data <- students_data[, sapply(students_data, is.numeric)]

# Perform PCA on the numerical data (standardized)
pca_model <- prcomp(numerical_data, scale. = TRUE)

# Summary of PCA model to get the proportion of variance explained by each component
summary(pca_model)

# Plot the explained variance (Scree plot)
variances <- summary(pca_model)$importance[2,]  # Proportion of variance (row 2 of the summary)

# Plot the scree plot (proportion of variance explained by each component)
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

# Biplot of the first two principal components
biplot(pca_model, main = "PCA Biplot")

# Display the loadings of the first two principal components
pca_model$rotation[, 1:2]

