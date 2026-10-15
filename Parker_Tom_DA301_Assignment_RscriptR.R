# LSE DA301 - Turtle Games Assignment

# What this script does:
# - Imports and checks the Turtle Games customer review data
# - Cleans the data and creates descriptive summaries
# - Explores distributions, relationships, outliers and group differences
# - Builds and evaluates a multiple linear regression model
# - Checks important model assumptions and exports useful results


### 1. set up and instaling


library(tidyverse)
library(skimr)
library(broom)
library(car)
library(lmtest)

### 2. import the data and quick look

# Check where R is currently looking for files.
getwd()

# read the original cvs. = FALSE keeps the console output tidy
reviews <- read_csv("turtle_reviews.csv", show_col_types = FALSE)

# Have a quick look at the data before changing 
head(reviews)
glimpse(reviews)
dim(reviews)
names(reviews)
summary(reviews)
skim(reviews)

# check what missing values are in each column
missing_before_cleaning <- tibble(
  column = names(reviews),
  missing_values = colSums(is.na(reviews))
)
print(missing_before_cleaning)

# check  rows for exact duplicates
duplicate_rows <- sum(duplicated(reviews))
cat("Number of exact duplicate rows:", duplicate_rows, "\n")

# keep a copy of the original data unchanged so cleaning can be checked
reviews_original <- reviews


### 3. Clean and prepare the data

# rename the column names so they are easier to use 
reviews_clean <- reviews %>%
  select(-any_of(c("language", "platform"))) %>%
  rename(
    remuneration = `remuneration (k£)`,
    spending_score = `spending_score (1-100)`
  )

# Check the cleaned data and make sure the key columns are there
glimpse(reviews_clean)
names(reviews_clean)

required_columns <- c(
  "loyalty_points",
  "age",
  "remuneration",
  "spending_score"
)

missing_required_columns <- setdiff(required_columns, names(reviews_clean))

if (length(missing_required_columns) > 0) {
  stop(
    "These required columns were not found: ",
    paste(missing_required_columns, collapse = ", "),
    ". Check the CSV column names before continuing."
  )
}

# check missing values after cleaning
missing_after_cleaning <- tibble(
  column = names(reviews_clean),
  missing_values = colSums(is.na(reviews_clean))
)
print(missing_after_cleaning)

# Save the cleaned version so it can be reused without repeating the cleaning.
write_csv(reviews_clean, "turtle_reviews_clean.csv")


### 4. Descriptive statistics for the main numeric variables

#select the four fields most relevant to loyalty points
numeric_data <- reviews_clean %>%
  select(loyalty_points, age, remuneration, spending_score)

# Create a  table with count, missing values, mean, median, standard deviation, minimum, quartiles and maximum for each variable
numeric_summary <- numeric_data %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
        q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

print(numeric_summary)

# create a cleaner table. Renaming titles to read better. No missing collomn as the vale is 0
numeric_summary_report <- numeric_summary_long %>%
  select(variable, mean, median, sd, min, q1, q3, max) %>%
  mutate(
    variable = case_when(
      variable == "loyalty_points" ~ "Loyalty points",
      variable == "age" ~ "Age",
      variable == "remuneration" ~ "Remuneration (£k)",
      variable == "spending_score" ~ "Spending score",
      TRUE ~ variable
    ),
    across(
      c(mean, median, sd, min, q1, q3, max),
      ~ round(.x, 2)
    )
  )

print(numeric_summary_report)

# loyalty points extra because they are the outcome we want to understand and later predict
loyalty_summary <- reviews_clean %>%
  summarise(
    n = sum(!is.na(loyalty_points)),
    missing = sum(is.na(loyalty_points)),
    mean = mean(loyalty_points, na.rm = TRUE),
    median = median(loyalty_points, na.rm = TRUE),
    sd = sd(loyalty_points, na.rm = TRUE),
    min = min(loyalty_points, na.rm = TRUE),
    q1 = quantile(loyalty_points, 0.25, na.rm = TRUE),
    q3 = quantile(loyalty_points, 0.75, na.rm = TRUE),
    max = max(loyalty_points, na.rm = TRUE)
  )

print(loyalty_summary)


###5. explore customer groups


# group summaries can help show whether loyalty points differ acrosscustomer groups.
if ("gender" %in% names(reviews_clean)) {
  gender_summary <- reviews_clean %>%
    group_by(gender) %>%
    summarise(
      customers = n(),
      mean_loyalty_points = mean(loyalty_points, na.rm = TRUE),
      median_loyalty_points = median(loyalty_points, na.rm = TRUE),
      mean_spending_score = mean(spending_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_loyalty_points))

  print(gender_summary)
  write_csv(gender_summary, "week5_gender_summary.csv")
}

if ("education" %in% names(reviews_clean)) {
  education_summary <- reviews_clean %>%
    group_by(education) %>%
    summarise(
      customers = n(),
      mean_loyalty_points = mean(loyalty_points, na.rm = TRUE),
      median_loyalty_points = median(loyalty_points, na.rm = TRUE),
      mean_spending_score = mean(spending_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_loyalty_points))

  print(education_summary)
  write_csv(education_summary, "week5_education_summary.csv")
}

#create three spending score bands for easy comparison
# these are same width bands across the observed range - the number of customers in each band may differ.
reviews_clean <- reviews_clean %>%
  mutate(
    spending_band = cut(
      spending_score,
      breaks = 3,
      include.lowest = TRUE,
      labels = c("Lower spending score", "Middle spending score",
                 "Higher spending score")
    )
  )

spending_summary <- reviews_clean %>%
  group_by(spending_band) %>%
  summarise(
    customers = n(),
    mean_loyalty_points = mean(loyalty_points, na.rm = TRUE),
    median_loyalty_points = median(loyalty_points, na.rm = TRUE),
    mean_remuneration = mean(remuneration, na.rm = TRUE),
    .groups = "drop"
  )

print(spending_summary)
write_csv(spending_summary, "week5_spending_summary.csv")

remuneration_summary <- reviews_clean %>%
  group_by(remuneration) %>%
  summarise(
    customers = n(),
    mean_loyalty_points = mean(loyalty_points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(remuneration)

# scatterplot easier to interpret
print(head(remuneration_summary, 10))
write_csv(remuneration_summary, "week5_remuneration_summary.csv")


### 6. Data Vis and exploration - distributions

# istograms help see the shape of a variable - skew and  narrow or broad range
# steel blue used as recommended by Okabe-Ito colour blind friendly palette
loyalty_histogram <- ggplot(reviews_clean, aes(x = loyalty_points)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    colour = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of customer loyalty points",
    x = "Loyalty points",
    y = "Number of customers"
  )

print(loyalty_histogram)


age_histogram <- ggplot(reviews_clean, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  theme_minimal() +
  labs(title = "Distribution of customer age",
       x = "Age", y = "Number of customers")
print(age_histogram)

remuneration_histogram <- ggplot(reviews_clean, aes(x = remuneration)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  theme_minimal() +
  labs(title = "Distribution of remuneration",
       x = "Remuneration (k£)", y = "Number of customers")
print(remuneration_histogram)

spending_histogram <- ggplot(reviews_clean, aes(x = spending_score)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  theme_minimal() +
  labs(title = "Distribution of spending score",
       x = "Spending score", y = "Number of customers")
print(spending_histogram)


### 7.0. Data Vis exploration - boxplots and outliers

# Boxplots give quick view of spread and possible outliers.
# A point outside the whiskers is not automatically an error. it should be investigated and only removed if there is a clear reason.
#light blue also color blind freindly 

loyalty_boxplot <- ggplot(reviews_clean, aes(y = loyalty_points)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Potential outliers in loyalty points", y = "Loyalty points")
print(loyalty_boxplot)

age_boxplot <- ggplot(reviews_clean, aes(y = age)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Potential outliers in age", y = "Age")
print(age_boxplot)

remuneration_boxplot <- ggplot(reviews_clean, aes(y = remuneration)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Potential outliers in remuneration", y = "Remuneration (£k)")
print(remuneration_boxplot)

spending_boxplot <- ggplot(reviews_clean, aes(y = spending_score)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Potential outliers in spending score", y = "Spending score")
print(spending_boxplot)

### 7.1. Boxplots for report

par(mfrow = c(2, 2))
par(mar = c(4, 4, 3, 1))

boxplot(
  reviews_clean$loyalty_points,
  main = "Loyalty Points",
  ylab = "Loyalty points",
  col = "lightblue"
)

boxplot(
  reviews_clean$age,
  main = "Age",
  ylab = "Age",
  col = "lightblue"
)

boxplot(
  reviews_clean$remuneration,
  main = "Remuneration",
  ylab = "Remuneration (£k)",
  col = "lightblue"
)

boxplot(
  reviews_clean$spending_score,
  main = "Spending Score",
  ylab = "Spending score",
  col = "lightblue"
)

par(mfrow = c(1, 1))


### 8. data vis exploration - relationships and correlations


# Scatterplots show  loyalty points rise or fall as each numeric feature changes 
# blue line is a simple linear trend - not proof
age_loyalty_plot <- ggplot(
  reviews_clean,
  aes(x = age, y = loyalty_points)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  theme_minimal() +
  labs(title = "Age and loyalty points", x = "Age", y = "Loyalty points")
print(age_loyalty_plot)

remuneration_loyalty_plot <- ggplot(
  reviews_clean,
  aes(x = remuneration, y = loyalty_points)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  theme_minimal() +
  labs(title = "Remuneration and loyalty points",
       x = "Remuneration (k£)", y = "Loyalty points")
print(remuneration_loyalty_plot)

spending_loyalty_plot <- ggplot(
  reviews_clean,
  aes(x = spending_score, y = loyalty_points)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  theme_minimal() +
  labs(title = "Spending score and loyalty points",
       x = "Spending score", y = "Loyalty points")
print(spending_loyalty_plot)

# correlation measures the strength and direction of a linear relationship - does not establish causation.
correlation_data <- reviews_clean %>%
  select(loyalty_points, age, remuneration, spending_score)

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)
print(round(correlation_matrix, 2))

#8.1. graphs for report
#one figure containing all three predictor relationships

par(mfrow = c(3, 1))
par(mar = c(4, 4, 2, 1))

plot(
  reviews_clean$age,
  reviews_clean$loyalty_points,
  pch = 16,
  col = "grey50",
  main = "Age and loyalty points",
  xlab = "Age",
  ylab = "Loyalty points"
)
abline(lm(loyalty_points ~ age, data = reviews_clean),
       col = "steelblue", lwd = 2)


plot(
  reviews_clean$remuneration,
  reviews_clean$loyalty_points,
  pch = 16,
  col = "grey50",
  main = "Remuneration and loyalty points",
  xlab = "Remuneration (£k)",
  ylab = "Loyalty points"
)
abline(lm(loyalty_points ~ remuneration, data = reviews_clean),
       col = "steelblue", lwd = 2)


plot(
  reviews_clean$spending_score,
  reviews_clean$loyalty_points,
  pch = 16,
  col = "grey50",
  main = "Spending score and loyalty points",
  xlab = "Spending score",
  ylab = "Loyalty points"
)
abline(lm(loyalty_points ~ spending_score, data = reviews_clean),
       col = "steelblue", lwd = 2)


par(mfrow = c(1, 1))

### 9. prepping modelling data

# For regression, keep the outcome and three predictors specified for his analysis
#rows missing any of these fields are removed for modelling only
model_data <- reviews_clean %>%
  select(loyalty_points, age, remuneration, spending_score) %>%
  drop_na()

# check if the modelling data set is big enough and contains variation
cat("Rows available for modelling:", nrow(model_data), "\n")
cat("Rows removed because of missing model fields:",
    nrow(reviews_clean) - nrow(model_data), "\n")

if (nrow(model_data) < 20) {
  stop("There are too few complete rows for a reliable train analysis.")
}

if (any(map_lgl(model_data, ~ n_distinct(.x) < 2))) {
  stop("At least one modelling variable has no variation. Check the data.")
}

# use a fixed seed so the same random train split can be reproduced
#test set is kept aside until evaluation to give a fairer estimate of model
set.seed(42)
train_rows <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.70 * nrow(model_data))
)

train_data <- model_data[train_rows, ]
test_data <- model_data[-train_rows, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")

#9.1.
final_model <- lm(
  loyalty_points ~ age + remuneration + spending_score,
  data = train_data
)

# View model fit
summary(final_model)


### 10. fit the multiple linear regression model

# use loyalty points as the outcome and age, remuneration and spending score as predictors. 
# This gives each coefficient an adjusted interpretation
# its relationship with loyalty points while holding the other predictors constant
loyalty_model <- lm(
  loyalty_points ~ age + remuneration + spending_score,
  data = train_data
)

# read this output for coefficient estimates, p values, R squared, adjusted R squared and residual standard error
summary(loyalty_model)

# tidy tables are easier to export and quote
model_coefficients <- tidy(
  loyalty_model,
  conf.int = TRUE
)
print(model_coefficients)

model_fit_summary <- glance(loyalty_model)
print(model_fit_summary)


### 11. Evaluate the model on the held-out test set

# generate predictions for customers that were not used to fit the model
test_predictions <- predict(loyalty_model, newdata = test_data)

# RMSE and MAE describe the typical size of prediction errors in loyalty points
#lower values are generally better when comparing models on the same test set
#test R squared compares prediction errors with predicting the test set mean so it can be negative if the model performs poorly.
actual_values <- test_data$loyalty_points
prediction_errors <- actual_values - test_predictions

test_performance <- tibble(
  n_test = length(actual_values),
  RMSE = sqrt(mean(prediction_errors^2)),
  MAE = mean(abs(prediction_errors)),
  test_R_squared = 1 -
    sum(prediction_errors^2) /
    sum((actual_values - mean(actual_values))^2)
)

print(test_performance)

# plot observed values against predictions/ perfect predictions would sit on the diagonal. large departures suggest prediction errors
prediction_plot_data <- tibble(
  actual = actual_values,
  predicted = as.numeric(test_predictions)
)

actual_vs_predicted_plot <- ggplot(
  prediction_plot_data,
  aes(x = actual, y = predicted)
) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "lightblue") +
  theme_minimal() +
  labs(
    title = "Actual vs predicted loyalty points: test data",
    x = "Actual loyalty points",
    y = "Predicted loyalty points"
  )
print(actual_vs_predicted_plot)


### 12. refit the final model on all complete rows

#once test performance has been measured, refit the chosen model using all complete observations for the final coefficient estimates.
# test-set metrics are retained as the unbiased performance assessment
final_model <- lm(
  loyalty_points ~ age + remuneration + spending_score,
  data = model_data
)

summary(final_model)

final_coefficients <- tidy(final_model, conf.int = TRUE)
final_model_fit <- glance(final_model)

print(final_coefficients)
print(final_model_fit)


### 13. multicollinearity and model assumptions 

# VIF checks whether predictors overlap strongly with one another.
#values around 5 or above are often a prompt to investigate - not to remove 
vif_values <- car::vif(final_model)
print(vif_values)

# diagnostic plots help identify non-linearity, unequal residual spread, unusual observations and departures from residual normality.
# Interpret the shapes rather than relying on a single test or threshold.
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1))
plot(final_model)
par(mfrow = c(1, 1))

# The Breusch-Pagan test checks for evidence of non-constant residual variance. 
# small p value suggests heteroscedasticity may be present
breusch_pagan_result <- lmtest::bptest(final_model)
print(breusch_pagan_result)

# The Shapiro-Wilk test checks residual normality. 
#it s sensitive to sample size, so interpret it alongside the QQplot
# R's Shapiro-Wilk test supports between 3 and 5,000 observations
model_residuals <- residuals(final_model)

if (length(model_residuals) >= 3 && length(model_residuals) <= 5000) {
  shapiro_result <- shapiro.test(model_residuals)
  print(shapiro_result)
} else {
  message(
    "Shapiro-Wilk test skipped because the residual count is outside 3 to 5000."
  )
}

# residuals should be roughly centered around zero with no clear curve or funnel shape.
# Helps assess linearity and whether residual variance is reasonably constant
residual_plot_data <- tibble(
  fitted = fitted(final_model),
  residual = residuals(final_model)
)

residuals_vs_fitted_plot <- ggplot(
  residual_plot_data,
  aes(x = fitted, y = residual)
) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgreen") +
  geom_smooth(se = FALSE, method = "loess") +
  theme_minimal() +
  labs(
    title = "Residuals vs fitted values",
    x = "Fitted loyalty points",
    y = "Residual"
  )
print(residuals_vs_fitted_plot)


### 14. Predicting loyalty points - illustrative customer scenarios

# Use realistic values taken from the data rather than inventing
# scenarios change all three predictors together - show example profiles, not the isolated effect of single predictor 
scenario_data <- tibble(
  scenario = c("Lower quartile profile", "Median profile",
               "Upper quartile profile"),
  age = as.numeric(quantile(model_data$age, c(0.25, 0.50, 0.75))),
  remuneration = as.numeric(
    quantile(model_data$remuneration, c(0.25, 0.50, 0.75))
  ),
  spending_score = as.numeric(
    quantile(model_data$spending_score, c(0.25, 0.50, 0.75))
  )
)

#  Confidence intervals describe uncertainty around the estimated mean
#  Prediction intervals are wider because they also allow for variation
scenario_predictions <- predict(
  final_model,
  newdata = scenario_data,
  interval = "prediction",
  level = 0.95
)

scenario_results <- bind_cols(
  scenario_data,
  as_tibble(scenario_predictions) %>%
    rename(
      predicted_loyalty_points = fit,
      prediction_lower_95 = lwr,
      prediction_upper_95 = upr
    )
)

print(scenario_results)

scenario_plot <- ggplot(
  scenario_results,
  aes(x = scenario, y = predicted_loyalty_points)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = prediction_lower_95, ymax = prediction_upper_95),
    width = 0.15
  ) +
  theme_minimal() +
  labs(
    title = "Predicted loyalty points for example customer profiles",
    subtitle = "Error bars show 95% prediction intervals",
    x = "Example profile",
    y = "Predicted loyalty points"
  )
print(scenario_plot)

# If you want to isolate one predictor, vary that feature while holding the
# other two at their medians. This makes the comparison easier to interpret.
age_scenarios <- tibble(
  age = as.numeric(quantile(model_data$age, c(0.25, 0.50, 0.75))),
  remuneration = median(model_data$remuneration),
  spending_score = median(model_data$spending_score)
)

age_scenario_predictions <- predict(
  final_model,
  newdata = age_scenarios,
  interval = "prediction"
)

age_scenario_results <- bind_cols(
  age_scenarios,
  as_tibble(age_scenario_predictions)
)
print(age_scenario_results)


### 15. Export key results

# Save main tables so the values can be checked and used in the report
write_csv(numeric_summary, "week5_numeric_summary.csv")
write_csv(loyalty_summary, "week5_loyalty_summary.csv")
write_csv(iqr_outlier_summary, "week5_iqr_outlier_summary.csv")
write_csv(as.data.frame(correlation_matrix) %>%
            rownames_to_column("variable"),
          "week5_correlation_matrix.csv")
write_csv(model_coefficients, "week6_training_model_coefficients.csv")
write_csv(final_coefficients, "week6_final_model_coefficients.csv")
write_csv(model_fit_summary, "week6_training_model_fit.csv")
write_csv(final_model_fit, "week6_final_model_fit.csv")
write_csv(test_performance, "week6_test_performance.csv")
write_csv(scenario_results, "week6_scenario_predictions.csv")
write_csv(age_scenario_results, "week6_age_scenario_predictions.csv")
write_csv(missing_before_cleaning, "missing_values_before_cleaning.csv")
write_csv(missing_after_cleaning, "missing_values_after_cleaning.csv")
