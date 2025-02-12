install.packages("caret", dependencies=TRUE)
library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)

# Read dataset
df <- read.csv("C:/Users/tolan/Downloads/Salary Data.csv", stringsAsFactors = TRUE)

# Display first few rows
head(df)

# Check dataset structure
str(df)

#Basic Data Exploration
# Summary of the dataset
summary(df)

# Check for missing values
colSums(is.na(df))

# Check unique values in categorical columns
sapply(df, function(x) if(is.factor(x)) unique(x) else NULL)

#Exploratory Data Analysis (EDA)
#Correlation Heatmap
# Compute correlation matrix
num_df <- df %>% select(Age, Years.of.Experience, Salary)
cor_matrix <- cor(num_df)

# Plot heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, main = "Feature Correlation Heatmap")

#Salary by Education Level
ggplot(df, aes(x = Education.Level, y = Salary, fill = Education.Level)) +
  geom_boxplot() +
  ggtitle("Salary Distribution by Education Level") +
  xlab("Education Level") +
  ylab("Standardized Salary") +
  theme_minimal()

#Regression Analysis

#Simple Linear Regression (Experience vs Salary)

# Linear Model
lm_model <- lm(Salary ~ Years.of.Experience, data = df)

# Model Summary
summary(lm_model)

# Visualize Regression Line
ggplot(df, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Linear Regression: Experience vs Salary") +
  xlab("Years of Experience (Standardized)") +
  ylab("Salary (Standardized)") +
  theme_minimal()


#Multiple Linear Regression (Including Age & Education)
# Fit Multiple Regression Model
multi_lm <- lm(Salary ~ Years.of.Experience + Age + Education.Level, data = df)

# Model Summary
summary(multi_lm)

#Model Evaluation
# Predict on dataset
# Ensure factor levels in Education.Level are fixed
edu_levels <- levels(df$Education.Level)
df$Education.Level <- factor(df$Education.Level, levels = edu_levels)

# Fit Multiple Regression Model
multi_lm <- lm(Salary ~ Years.of.Experience + Age + Education.Level, data = df)

# Predict on dataset
df$Predicted_Salary <- predict(multi_lm, df)

# Convert predicted salary back to numeric if necessary
df$Predicted_Salary <- as.numeric(df$Predicted_Salary)

# Calculate R-squared and RMSE
actual <- as.numeric(df$Salary)  # Ensure Salary is numeric
predicted <- df$Predicted_Salary

r_squared <- cor(actual, predicted, use="complete.obs")^2
rmse <- sqrt(mean((actual - predicted)^2))

print(paste("R-squared:", round(r_squared, 3)))
print(paste("RMSE:", round(rmse, 3)))









