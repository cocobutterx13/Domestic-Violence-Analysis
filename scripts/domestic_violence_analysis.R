# =============================================================================
# DOMESTIC VIOLENCE ANALYSIS
# =============================================================================
# Project:   Domestic Violence Against Women — Socioeconomic and Demographic Factors
# Dataset:   347 observations, 5 predictors + 1 binary outcome
# Variables: Age, Education, Employment, Income, Marital Status -> Violence (yes/no)
# Goal:      Explore how education, employment, income, age, and marital status
#            relate to the likelihood of experiencing domestic violence.
# Author:    Caylin Manuel
# Date:      April 2026
# =============================================================================


# =============================================================================
# TABLE OF CONTENTS
# =============================================================================
# SECTION 1 - Setup
# SECTION 2 - Data Import & Cleaning
# SECTION 3 - Exploratory Data Analysis (EDA)
# SECTION 4 - Statistical Inference
# SECTION 5 - Logistic Regression
#     5.1. Univariate Logistic Regression
#     5.2. Full Multivariable Logistic Regression
#     5.3. Collinearity Check (VIF)
#     5.4. Forest Plot - Odds Ratios
# SECTION 6 - Model Diagnostics
#     6.1. Predicted Probabilities
#     6.2. Confusion Matrix at 0.5 Threshold
# SECTION 7 - Summary
# =============================================================================


# =============================================================================
# SECTION 1 - SETUP
# =============================================================================
install.packages(c("tidyverse", "ggridges"))

library(tidyverse)
library(ggridges)

# Set a consistent ggplot theme for all visualisations
theme_set(
  theme_minimal(base_size = 13) +
    theme(
      # Titles
      plot.title       = element_text(face = "bold", size = 16, colour = "#111111"),
      plot.subtitle    = element_text(colour = "#555555", size = 12),
      
      # Axes
      axis.title       = element_text(size = 11, colour = "#222222"),
      axis.text        = element_text(colour = "#333333"),
      
      # Gridlines
      panel.grid.major = element_line(colour = "#e6e6e6", linewidth = 0.6),
      panel.grid.minor = element_blank(),
      
      # Background tint
      panel.background = element_rect(fill = "#fafafa", colour = NA),
      plot.background  = element_rect(fill = "#fafafa", colour = NA),
      
      #Legend
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      
      plot.margin      = margin(15, 15, 15, 15)
    )
)

# Colour palette: two colours for the Violence outcome
violence_colours <- c("yes" = "#ff1493", "no" = "#00ff00")


# =============================================================================
# SECTION 2 — DATA IMPORT & CLEANING
# =============================================================================

# --- 2.1. Read the raw CSV ---
raw_data <- read.csv("data/Domestic_violence.csv")


# --- 2.2. Inspect what was loaded ---
glimpse(raw_data)
summary(raw_data)
head(raw_data)


# -- 2.3. Clean column names ---
dv <- raw_data %>%
  rename_with(str_trim) %>%           # Remove whitespace from all names
  rename(
    id             = `SL..No`,
    age            = Age,
    education      = Education,
    employment     = Employment,
    income         = Income,
    marital_status = `Marital.status`,
    violence       = Violence
  )


# --- 2.4. Tidy string columns ---
dv <- dv %>%
  mutate(
    # Standardise case and strip stray spaces in character columns
    across(where(is.character), str_trim),
    across(where(is.character), str_to_lower),
    
    # Fix the "employed " duplicate caused by trailing space in raw data
    employment = str_trim(employment),
    
    # Note: "unmarred" in source; convert all instances to "unmarried"
    marital_status = str_replace(marital_status, "unmarred", "unmarried"),
    
    # Convert outcome and categorical predictors to factors with logical level order
    violence       = factor(violence, 
                            levels = c("yes", "no")),
    education      = factor(education, 
                            levels = c("none", "primary", "secondary", "tertiary")),
    employment     = factor(employment, 
                            levels = c("unemployed", "semi employed", "employed")),
    marital_status = factor(marital_status, 
                            levels = c("married", "unmarried")) 
  )


# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS (EDA)
# =============================================================================

# --- 3.1. Outcome distribution: Bar Graph ---
# Summary of Violence outcome
violence_summary <- dv %>%
  count(violence) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(violence_summary, aes(x = violence, y = n, fill = violence)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_label(
    aes(label = paste0(n, "\n", pct, "%")),
    vjust    = 1.2,
    size     = 4.5,
    fontface = "bold",
    fill     = "white",
    colour   = "#111111",
    label.size = 0
  ) +
  scale_fill_manual(values = violence_colours) +
  labs(
    title    = "Distribution of Domestic Violence Outcome",
    subtitle = "Sample is imbalanced — roughly 3:1 non-violent to violent",
    x        = "Experienced violence",
    y        = "Number of Women"
  )
ggsave("outputs/01_outcome_distribution.png", width = 6, height = 5, dpi = 150)
   

# --- 3.2. Age distribution by outcome: Histogram ---
ggplot(dv, aes(x = age, fill = violence)) +
  geom_histogram(binwidth = 5, colour = "white", alpha = 0.6,
                 position = "identity") +
  scale_fill_manual(values = violence_colours, name = "Violence") +
  labs(
    title = "Age Distribution by Violence Outcome",
    subtitle = "Overlapping histograms; bin width = 5 years",
    x = "Age (years)",
    y = "Count"
  )
ggsave("outputs/02_age_distribution.png", width = 8, height = 5, dpi = 150)

# Summary statistics for age by outcome
dv %>%
  group_by(violence) %>%
  summarise(
    n      = n(),
    mean   = mean(age),
    median = median(age),
    sd     = sd(age),
    min    = min(age),
    max    = max(age)
  )


# --- 3.3a. Education vs violence: Bar Chart ---
edu_violence <- dv %>%
  count(education, violence) %>%
  group_by(education) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(edu_violence, aes(x = education, y = pct, fill = violence)) +
  geom_col(position = "stack", colour = "white", linewidth = 0) +
  scale_fill_manual(values = violence_colours, name = "Violence") +
  labs(
    title    = "Violence Rate by Education Level",
    subtitle = "Stacked bars show percentage within each education group",
    x        = "Education level",
    y        = "Percentage (%)"
  )
ggsave("outputs/03_education_vs_violence_bar.png", width = 7, height = 5, dpi = 150)


# --- 3.3b. Education vs violence: Lollipop Chart ---
edu_lollipop <- dv %>%
  group_by(education) %>%
  summarise(rate = mean(violence == "yes") * 100)

ggplot(edu_lollipop, aes(x = education, y = rate)) +
  geom_segment(
    aes(xend = education, y = 0, yend = rate),
    colour    = "#cccccc",
    linewidth = 1.2
  ) +
  geom_point(size = 5, colour = "#ff1493") +
  geom_text(
    aes(label = paste0(round(rate, 1), "%")),
    vjust = -1, size = 3.8, colour = "#333333"
  ) +
  ylim(0, max(edu_lollipop$rate) + 5) +
  labs(
    title    = "Violence Rate by Education Level",
    subtitle = "Percentage of women in each group who experienced violence",
    x        = "Education level",
    y        = "Violence rate (%)"
  )
ggsave("outputs/04_education_lollipop.png", width = 7, height = 5, dpi = 150)


# --- 3.4. Employment vs violence: Line Graph ---
emp_violence <- dv %>%
  count(employment, violence) %>%
  group_by(employment) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(emp_violence, aes(x = employment, y = pct,
                      group = violence, colour = violence)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4) +
  geom_text(
    data = emp_violence %>% filter(employment == "employed"),
    aes(label = paste0(ifelse(violence == "yes", "Violence", "No violence"),
                       "  ", round(pct, 1), "%")),
    hjust = -0.15, size = 3.5, fontface = "bold"
  ) +
  scale_colour_manual(values = violence_colours, guide = "none") +
  scale_x_discrete(
    expand = expansion(mult = c(0.1, 0.35))
  ) +
  labs(
    title    = "Violence Proportion by Employment Status",
    x        = "Employment status",
    y        = "Percentage (%)"
  )
ggsave("outputs/05_employment_vs_violence.png", width = 7, height = 5, dpi = 150)


# --- 3.5. Income distribution: Density Graph ---
# Log1p transform handles the many zero-income observations
ggplot(dv, aes(x = income, fill = violence)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(trans = "log1p", breaks = c(0, 1000, 5000, 10000, 20000)) +
  scale_fill_manual(values = violence_colours) +
  labs(
    title = "Income Distribution by Violence Outcome",
    x = "Income",
    y = "Density" # Proportion of people who experienced violence
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("outputs/06_income_density.png", width = 8, height = 5, dpi = 150)

# Income is heavily non-normal, justifying non-parametric methods / log transformation
ggplot(dv, aes(sample = income)) +
  stat_qq() +
  stat_qq_line(colour = "#ff1493") +
  labs(title = "QQ Plot of Income (Normality Check)")
ggsave("outputs/07_income_qqplot.png", width = 8, height = 5, dpi = 150)

# Median income comparison
### Verify values
dv %>%
  group_by(violence) %>%
  summarise(
    n               = n(),
    pct_zero_income = mean(income == 0) * 100,
    median_income   = median(income),
    mean_income     = mean(income)
  )


# --- 3.6. Income distribution across education levels: Density Graph ---
ggplot(dv, aes(x = income, y = education, fill = violence)) +
  geom_density_ridges(alpha = 0.6, scale = 1.1) +
  scale_x_continuous(
    trans = "log1p",
    breaks = c(0, 1000, 5000, 10000, 20000),
  ) +
  scale_fill_manual(values = violence_colours) +
  labs(
    title = "Income Distribution Across Education Levels",
    x = "Income",
    y = "Education",
    fill = "Violence"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("outputs/08_income_vs_education_density.png", width = 8, height = 5, dpi = 150)


# --- 3.7. Marital status vs violence: Stacked Bar Graph ---
marital_violence <- dv %>%
  count(marital_status, violence) %>%
  group_by(marital_status) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(marital_violence, aes(x = marital_status, y = pct, fill = violence)) +
  geom_col(position = "stack", colour = "white") +
  scale_fill_manual(values = violence_colours, name = "Violence") +
  labs(
    title = "Violence Rate by Marital Status",
    x     = "Marital status",
    y     = "Percentage (%)"
  )
ggsave("outputs/09_marital_status_vs_violence.png", width = 6, height = 5, dpi = 150)


# --- 3.8. Age vs Income by Violence Outcome: Scatter Plot ---
ggplot(dv, aes(x = age, y = income, colour = violence)) +
  geom_jitter(alpha = 0.7, width = 0.5, height = 0) +
  scale_colour_manual(values = violence_colours) +
  labs(
    title = "Age vs Income by Violence Outcome",
    x = "Age",
    y = "Income"
  )
ggsave("outputs/10_age_vs_income_by_violence.png", width = 6, height = 5, dpi = 150)


# =============================================================================
# SECTION 4: STATISTICAL INFERENCE
# =============================================================================
# Test whether the observed associations are statistically significant.
# H0: no association between predictor and violence.
# H1: there is association between predictor and violence.
# A p-value < 0.05 suggests that there is sufficient evidence to reject H0.

# --- 4.1. Chi-squared tests for categorical predictors vs outcome ---
# Create a function to run the Chi-squared test and conclusion for each predictor
report_chisq <- function(var_name, var, outcome, alpha = 0.05) {
  test <- chisq.test(table(var, outcome))
  p_val <- test$p.value
  
  cat("\n--- Chi-square:", var_name, "vs Violence ---\n")
  cat("p-value:", round(p_val, 4), "\n")
  
  if (p_val < alpha) {
    cat("Result: p-value <", alpha, "\u2234 statistically significant.\n")
    cat("Conclusion: Sufficient evidence to reject H0 (there is an association.\n")
  } else {
    cat("Result: p-value >=", alpha, "\u2234 not statistically significant.\n")
    cat("Conclusion: Fail to reject H0 (insufficient evidence of association.\n")
  }
}

# Education
report_chisq("Education", dv$education, dv$violence)

# Employment
report_chisq("Employment", dv$employment, dv$violence)

# Marital Status
report_chisq("Marital Status", dv$marital_status, dv$violence)


# --- 4.2. t-test: Age by violence outcome ---
report_ttest_age <- function(data, alpha = 0.05) {
  test <- t.test(age ~ violence, data = data)
  p_val <- test$p.value
  
  cat("\n--- Welch t-test: Age by Violence ---\n")
  cat("Mean age (violence = yes):",
      round(mean(data$age[data$violence == "yes"], na.rm = TRUE), 2), "\n")
  cat("Mean age (violence = no):",
      round(mean(data$age[data$violence == "no"], na.rm = TRUE), 2), "\n")
  cat("t =", round(test$statistic, 3),
      "| df =", round(test$parameter, 1), "\n")
  cat("p-value:", round(p_val, 4), "\n")
  
  if (p_val < alpha) {
    cat("Result: p-value <", alpha, "\u2234 statistically significant.\n")
    cat("Conclusion: Sufficient evidence to reject H0 (mean ages differ).\n")
  } else {
    cat("Result: p-value >=", alpha, "\u2234 not statistically significant.\n")
    cat("Conclusion: Fail to reject H0 (no evidence of difference in mean ages).\n")
  }
}

report_ttest_age(dv)


# --- 4.3. Wilcoxon rank-sum test: Income by violence outcome ---
# Income is heavily right-skewed with many zeros -> non-parametric test preferred
report_wilcox_income <- function(data, alpha = 0.05) {
  test <- wilcox.test(income ~ violence, data = data)
  p_val <- test$p.value
  
  cat("\n--- Wilcoxon rank-sum: Income by Violence ---\n")
  cat("Median income (violence = yes):",
      round(median(data$income[data$violence == "yes"], na.rm = TRUE), 2), "\n")
  cat("Median income (violence = no):",
      round(median(data$income[data$violence == "no"], na.rm = TRUE), 2), "\n")
  cat("W =", round(test$statistic, 3), "\n")
  cat("p-value:", round(p_val, 4), "\n")
  
  if (p_val < 0.05) {
    cat("Result: p-value <", alpha, "\u2234 statistically significant.\n")
    cat("Conclusion: Sufficient evidence to reject H0 (income distributions differ).\n")
  } else {
    cat("Result: p-value >=", alpha, "\u2234 not statistically significant.\n")
    cat("Conclusion: Fail to reject H0 (no evidence of difference in income distributions).\n")
  }
}

report_wilcox_income(dv)


# =============================================================================
# SECTION 5: LOGISTIC REGRESSION
# =============================================================================

# --- 5.1. Model fitting ---
# glm() requires the outcome to be coded 1/0; relevel so "yes" = 1
dv_model <- dv %>%
  mutate(
    violence_bin = if_else(violence == "yes", 1L, 0L),
    income_log = log1p(income) # log-transform income to reduce skew
  )

model <- glm(
  violence_bin ~ age + education + employment + income_log + marital_status,
  data   = dv_model,
  family = binomial(link = "logit")
)

cat("\n--- Logistic Regression Summary ---\n")
print(summary(model))


# --- 5.2.