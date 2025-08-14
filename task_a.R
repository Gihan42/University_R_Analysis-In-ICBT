# ====================================================
# STEP 1: INSTALL & LOAD REQUIRED PACKAGES
# ====================================================
required_packages <- c("ggplot2", "dplyr", "corrplot", "car")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# ====================================================
# STEP 2: SET WORKING DIRECTORY & LOAD DATA
# ====================================================
setwd("D:/icbt/galle/final r and qigs/galle assignment/data set/ABI Batch 07 assignment/ABI Batch 07 assignment/ABI-CIS6008-MAR-2025-Dataset/Question-(a)")
uqr <- read.csv("UQR.csv", stringsAsFactors = FALSE)


# ====================================================
# STEP 3: CLEANING & TRANSFORMING DATA
# ====================================================
numeric_cols <- c("Student.Enrollment", "Faculty.Salary..Avg..",
                  "Research.Funding..Million.USD.", "Graduation.Rate....",
                  "Student.Faculty.Ratio", "Tuition.Fees..USD.",
                  "Employment.Rate....", "University.Ranking.Score")

uqr[numeric_cols] <- lapply(uqr[numeric_cols], as.numeric)
uqr$Institution.Type <- as.factor(uqr$Institution.Type)
uqr <- na.omit(uqr)



# Histogram with Normal Curve for University.Ranking.Score
p_hist <- ggplot(uqr, aes(x = University.Ranking.Score)) +
  # Create the histogram, using '..density..' to plot density instead of count
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#87CEEB", color = "white") +
  # Overlay a normal distribution curve based on the data's mean and sd
  stat_function(
    fun = dnorm,
    args = list(mean = mean(uqr$University.Ranking.Score), sd = sd(uqr$University.Ranking.Score)),
    color = "#00008B", # Dark Blue
    size = 1.2
  ) +
  labs(
    title = "Histogram of University Ranking Score with Normal Curve",
    x = "University Ranking Score",
    y = "Density"
  ) +
  theme_bw() 

# Print and save the plot
print(p_hist)
ggsave("histogram_ranking_score.png", p_hist, width = 8, height = 6)


# Q-Q Plot for University.Ranking.Score
p_qq <- ggplot(uqr, aes(sample = University.Ranking.Score)) +
  stat_qq(color = "black") +      # The points representing the data quantiles
  stat_qq_line(color = "red") +   # The reference line for a normal distribution
  labs(
    title = "Q-Q Plot of University Ranking Score",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw()

# Print and save the plot
print(p_qq)
ggsave("qqplot_ranking_score.png", p_qq, width = 8, height = 6)



# ====================================================
# STEP 4: SUMMARY STATISTICS
# ====================================================
summary(uqr)
colnames(uqr)

# Correlation matrix with selected key variables
key_vars <- uqr[, c("University.Ranking.Score",
                    "Student.Enrollment",
                    "Faculty.Salary..Avg..",
                    "Research.Funding..Million.USD.",
                    "Graduation.Rate....",
                    "Tuition.Fees..USD.",
                    "Employment.Rate....")]

cor_matrix <- cor(key_vars)
print("Correlation Matrix:")
print(round(cor_matrix, 2))

# Visualize correlation heatmap
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, type = "lower",
           title = "Correlation Heatmap: University Quality Factors",
           lab_size = 3, colors = c("#6D9EC1", "white", "#E46726"))

# Enhanced scatter matrix for visualizing relationships
library(GGally)
quality_plot <- ggpairs(key_vars,
                        title = "University Quality Factors vs. Ranking Score",
                        progress = FALSE,
                        upper = list(continuous = "cor"),
                        lower = list(continuous = "smooth"))

# Save the plot if needed
ggsave("university_quality_factors.png", quality_plot, width = 10, height = 8)





# ====================================================
# STEP 5: CORRELATION MATRIX
# ====================================================
cor_matrix <- cor(uqr[numeric_cols], use = "complete.obs")

# Generate and save the correlation plot
png("correlation_plot.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "blue", tl.srt = 45, addCoef.col = "black",
         title = "Correlation Matrix of University Metrics", mar = c(0,0,2,0))
dev.off()


# ====================================================
# STEP 6: SCATTER PLOTS
# ====================================================
# 1. Research Funding vs Ranking Score
p1 <- ggplot(uqr, aes(x = Research.Funding..Million.USD., y = University.Ranking.Score)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "University Ranking Score vs Research Funding",
       x = "Research Funding (Million USD)", y = "University Ranking Score")
ggsave("ranking_vs_research_funding.png", p1)
print(p1)


# 2. Graduation Rate vs Ranking Score
p2 <- ggplot(uqr, aes(x = Graduation.Rate...., y = University.Ranking.Score)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "University Ranking Score vs Graduation Rate",
       x = "Graduation Rate (%)", y = "University Ranking Score")
ggsave("ranking_vs_graduation_rate.png", p2)
print(p2)



# 3. Boxplot by Institution Type
p3 <- ggplot(uqr, aes(x = Institution.Type, y = University.Ranking.Score, fill = Institution.Type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Public" = "steelblue", "Private" = "yellow")) +
  theme_minimal() +
  labs(title = "University Ranking Score by Institution Type",
       x = "Institution Type", y = "University Ranking Score")
ggsave("ranking_by_institution_type.png", p3)
print(p3)


# ====================================================
# STEP 7: MULTIPLE LINEAR REGRESSION
# ====================================================
lm_model <- lm(University.Ranking.Score ~ Student.Enrollment + Faculty.Salary..Avg.. +
                 Research.Funding..Million.USD. + Graduation.Rate.... +
                 Student.Faculty.Ratio + Tuition.Fees..USD. + Employment.Rate.... +
                 Institution.Type, data = uqr)

summary(lm_model)

# Save regression summary
sink("regression_summary.txt")
print(summary(lm_model))
sink()


# ====================================================
# STEP 8: DIAGNOSTIC PLOTS
# ====================================================
png("diagnostic_plots.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(lm_model)
dev.off()
par(mfrow = c(1, 1))

# ====================================================
# STEP 9: MULTICOLLINEARITY - VIF CHECK
# ====================================================
vif_result <- vif(lm_model)
print("VIF Results:")
print(vif_result)


# ====================================================
# STEP 10: EXTRA - VARIABLE-WISE SCATTER PLOTS
# ====================================================
# All predictors vs Ranking Score

model_residuals <- residuals(lm_model)


required_packages <- c("ggplot2", "dplyr", "corrplot", "car", "nortest", "nortest") 
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)



# 1. Shapiro-Wilk Test
shapiro_test <- shapiro.test(model_residuals)
print(shapiro_test)


# 2. Anderson-Darling Test
ad_test <- ad.test(model_residuals)
print(ad_test)



# 3. Lilliefors (Kolmogorov-Smirnov) Test
lillie_test <- lillie.test(model_residuals)
print(lillie_test)


# --- Visual Normality Check: Q-Q Plot and Histogram ---
# Generate and save a Q-Q plot of residuals
png("residuals_qq_plot.png", width = 600, height = 500)
qqnorm(model_residuals, main = "Normal Q-Q Plot of Residuals")
qqline(model_residuals, col = "red", lwd = 2)
dev.off()


# Generate and save a histogram of residuals
png("residuals_histogram.png", width = 600, height = 500)
hist(model_residuals, main = "Histogram of Residuals", xlab = "Residuals", border = "blue", col = "lightblue")
dev.off()

# ====================================================
# STEP 11: EXTRA - VARIABLE-WISE SCATTER PLOTS
# ====================================================
# Create scatter plots for all predictors vs. the ranking score
library(rlang)
predictors <- numeric_cols[-length(numeric_cols)] # Exclude the target variable

for (col in predictors) {
  # Generate plot
  p <- ggplot(uqr, aes_string(x = `col`, y = "University.Ranking.Score")) +
    geom_point(color = "green") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    theme_minimal() +
    labs(title = paste("University Ranking Score vs", col),
         x = col, y = "University Ranking Score")
  
  # Print and save the plot
  print(p)
  ggsave(filename = paste0("plot_", gsub("\\.|\\s|\\(|\\)", "_", col), "_vs_ranking.png"), plot = p, width = 8, height = 6)
}



#========================================================================================
#out put get to doc
#Descriptive Analysis 
library(dplyr)
install.packages("moments")

library(moments)

# Define a custom function to calculate mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Numeric columns to analyze
numeric_cols <- c("Student.Enrollment", "Faculty.Salary..Avg..",
                  "Research.Funding..Million.USD.", "Graduation.Rate....",
                  "Tuition.Fees..USD.", "Employment.Rate....", "University.Ranking.Score")


# Create summary table
summary_table <- data.frame(
  Variable = numeric_cols,
  Mean = sapply(uqr[numeric_cols], mean),
  Median = sapply(uqr[numeric_cols], median),
  Mode = sapply(uqr[numeric_cols], get_mode),
  Skewness = sapply(uqr[numeric_cols], skewness)
)

# Add interpretation of symmetry
summary_table$Symmetrical <- ifelse(
  abs(summary_table$Skewness) < 0.5, "Approximately Symmetrical",
  ifelse(summary_table$Skewness > 0, "Slightly Right-Skewed", "Slightly Left-Skewed")
)

# Print nicely
print(summary_table)


#Normal Distribution
# Load necessary libraries
library(nortest)
install.packages("tseries")
library(tseries)     # For Lilliefors test
library(stats)       # Shapiro-Wilk test (built-in)

# Example: Fit a linear model
model <- lm(`University.Ranking.Score` ~ `Graduation.Rate....` + `Employment.Rate....` + `Research.Funding..Million.USD.` + `Tuition.Fees..USD.`, data = uqr)

# Extract residuals
res <- residuals(model)

# 1. Shapiro-Wilk Test
shapiro_result <- shapiro.test(res)

# 2. Anderson-Darling Test
ad_result <- ad.test(res)

# 3. Lilliefors (Kolmogorov-Smirnov) Test
lillie_result <- lillie.test(res)

# Print results
cat("Shapiro-Wilk Test p-value:", shapiro_result$p.value, "\n")
cat("Anderson-Darling Test p-value:", ad_result$p.value, "\n")
cat("Lilliefors Test p-value:", lillie_result$p.value, "\n")


#Correlation Analysis
# Variables to correlate with University Ranking Score
predictors <- c("Student.Enrollment", "Faculty.Salary..Avg..", "Research.Funding..Million.USD.",
                "Graduation.Rate....", "Tuition.Fees..USD.", "Employment.Rate....")

# Initialize vectors to store results
correlations <- numeric(length(predictors))
p_values <- numeric(length(predictors))

for (i in seq_along(predictors)) {
  test <- cor.test(uqr[[predictors[i]]], uqr$University.Ranking.Score, method = "pearson")  # Or method = "spearman"
  correlations[i] <- test$estimate
  p_values[i] <- test$p.value
}

# Create a data frame with results
cor_results <- data.frame(
  Variable = predictors,
  Correlation_Coefficient = round(correlations, 2),
  P_Value = round(p_values, 4)
)

print(cor_results)



