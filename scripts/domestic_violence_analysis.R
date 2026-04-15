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
#     4.1. Chi-Square Tests (Categorical Variables)
#     4.2. Welch t-test (age)
#     4.3. Wilcoxon Rank-Sum Test (Income)
# SECTION 5 - Logistic Regression
#     5.1. Full Multivariable Logistic Regression
#     5.2. Correlation Matrix (Exploratory Multicollinearity Check)
#     5.3. Multicollinearity Check (GVIF)
#     5.4. Odds Ratio Table + Confidence Intervals
#     5.5. Forest Plot of Odds Ratios
# SECTION 6 - Model Diagnostics
#     6.1. Predicted Probabilities
#     6.2. Confusion Matrix at 0.5 Threshold
#     6.3. Classification Metrics
# SECTION 7 - Summary
# =============================================================================


# =============================================================================
# SECTION 1 - SETUP
# =============================================================================
install.packages(c(
  "tidyverse",
  "ggridges",
  "yardstick",
  "car",
  "broom",
  "gridExtra"
))

library(tidyverse)
library(ggplot2)
library(ggridges)
library(yardstick)
library(car)
library(reshape2)
library(gridExtra)

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

# Table-saving helper function
save_table_as_plot <- function(df, title, filename,
                               width = 8, height = NULL) {
  n_rows <- nrow(df)
  
  # Auto-size height: 0.5 in per row + 1.2 in for title + margins
  if(is.null(height)) height <- max(2.5, n_rows * 0.45 + 1.4)
  fill_colours <- rep(c("#fafafa", "#f0f0f0"), length.out = n_rows)
  
  tbl <- tableGrob(
    df,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 11,
      colhead   = list(
        bg_params = list(fill = "#ff1493", col = NA),
        fg_params = list(col = "white", fontface = "bold")
      ),
      core      = list(
        bg_params = list(fill = fill_colours, col = "white", lwd = 1),
        fg_params = list(col = "#111111")
      )
    )
  )
  
  title_grob <- grid::textGrob(
    title,
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#111111")
  )
  
  combined <- gridExtra::arrangeGrob(title_grob, tbl,
                                     nrow    = 2,
                                     heights = grid::unit(c(0.18, 0.82), "npc"))
  
  ggsave(
    filename = filename,
    plot     = combined,
    width    = width,
    height   = height,
    dpi      = 150,
    bg       = "#fafafa"
  )
  
  message("Saved: ", filename)
}


# =============================================================================
# SECTION 2 — DATA IMPORT & CLEANING
# =============================================================================

# --- 2.1. Read the raw CSV ---
raw_data <- read.csv("data/Domestic_violence.csv")


# --- 2.2. Inspect what was loaded ---
glimpse(raw_data)

summary(raw_data)

head(raw_data)
top_data <- as.data.frame(head(raw_data))
top_data <- tibble::rownames_to_column(top_data, "Variable")

save_table_as_plot(
  df       = top_data,
  title    = "First 6 Rows of Dataset",
  filename = "outputs/tables/01_data_head.png"
)

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
ggsave("outputs/plots/01_outcome_distribution.png", width = 6, height = 5, dpi = 150)
   

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
ggsave("outputs/plots/02_age_distribution.png", width = 8, height = 5, dpi = 150)

# Summary statistics for age by outcome
age_summary <- dv %>%
  group_by(violence) %>%
  summarise(
    n      = n(),
    mean   = mean(age),
    median = median(age),
    sd     = sd(age),
    min    = min(age),
    max    = max(age)
  )

save_table_as_plot(
  df       = age_summary,
  title    = "Age Summary by Violence Group",
  filename = "outputs/tables/02_age_summary.png"
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
ggsave("outputs/plots/03_education_vs_violence_bar.png", width = 7, height = 5, dpi = 150)


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
ggsave("outputs/plots/04_education_lollipop.png", width = 7, height = 5, dpi = 150)


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
ggsave("outputs/plots/05_employment_vs_violence.png", width = 7, height = 5, dpi = 150)


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
ggsave("outputs/plots/06_income_density.png", width = 8, height = 5, dpi = 150)

# Income is heavily non-normal, justifying non-parametric methods / log transformation
ggplot(dv, aes(sample = income)) +
  stat_qq() +
  stat_qq_line(colour = "#ff1493") +
  labs(title = "QQ Plot of Income (Normality Check)")
ggsave("outputs/plots/07_income_qqplot.png", width = 8, height = 5, dpi = 150)

# Median income comparison
income_summary <- dv %>%
  group_by(violence) %>%
  summarise(
    n               = n(),
    pct_zero_income = mean(income == 0) * 100,
    median_income   = median(income),
    mean_income     = mean(income)
  )

save_table_as_plot(
  df       = income_summary,
  title    = "Median Income Comparison by Violence Outcome",
  filename = "outputs/tables/03_income_summary.png"
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
ggsave("outputs/plots/08_income_vs_education_density.png", width = 8, height = 5, dpi = 150)


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
ggsave("outputs/plots/09_marital_status_vs_violence.png", width = 6, height = 5, dpi = 150)


# --- 3.8. Age vs Income by Violence Outcome: Scatter Plot ---
ggplot(dv, aes(x = age, y = income, colour = violence)) +
  geom_jitter(alpha = 0.7, width = 0.5, height = 0) +
  scale_colour_manual(values = violence_colours) +
  labs(
    title = "Age vs Income by Violence Outcome",
    x = "Age",
    y = "Income"
  )
ggsave("outputs/plots/10_age_vs_income_by_violence.png", width = 6, height = 5, dpi = 150)


# =============================================================================
# SECTION 4: STATISTICAL INFERENCE
# =============================================================================
# Test whether the observed associations are statistically significant.
# H0: no association between predictor and violence.
# H1: there is association between predictor and violence.
# A p-value < 0.05 suggests that there is sufficient evidence to reject H0.

# --- 4.1. Chi-Squared Tests (Categorical Variables) ---
# Create a function to run the Chi-squared test and conclusion for each predictor
report_chisq <- function(var_name, var, outcome, alpha = 0.05) {
  test  <- chisq.test(table(var, outcome))
  p_val <- test$p.value
  sig   <- p_val < alpha
  
  cat("\n--- Chi-square:", var_name, "vs Violence ---\n")
  cat("X\u00b2 =", round(test$statistic, 3),
      "| df =", test$parameter, "\n")
  cat("p-value:", round(p_val, 4), "\n")
  if (sig) {
    cat("Result: p-value <", alpha, "\u2234 statistically significant.\n")
    cat("Conclusion: Sufficient evidence to reject H0 (there is an association).\n")
  } else {
    cat("Result: p-value >=", alpha, "\u2234 not statistically significant.\n")
    cat("Conclusion: Fail to reject H0 (insufficient evidence of association).\n")
  }
  
  data.frame(
    Variable    = var_name,
    Chi_sq      = round(test$statistic, 3),
    df          = test$parameter,
    P_Value     = round(p_val, 4),
    Significant = ifelse(sig, "Yes ***", "No")
  )
}

# Run all three chi-square tests and collect results into one table
chisq_results <- bind_rows(
  report_chisq("Education",      dv$education,      dv$violence),
  report_chisq("Employment",     dv$employment,     dv$violence),
  report_chisq("Marital Status", dv$marital_status, dv$violence)
)

save_table_as_plot(
  df       = chisq_results,
  title    = "Chi-Square Tests: Categorical Predictors vs Violence",
  filename = "outputs/tables/04_chisq_results.png"
)


# --- 4.2. Welch t-test (Age) ---
# Returns a 1-row data frame AND prints to console
report_ttest_age <- function(data, alpha = 0.05) {
  test  <- t.test(age ~ violence, data = data)
  p_val <- test$p.value
  sig   <- p_val < alpha
  
  mean_yes <- round(mean(data$age[data$violence == "yes"], na.rm = TRUE), 2)
  mean_no  <- round(mean(data$age[data$violence == "no"],  na.rm = TRUE), 2)
  
  cat("\n--- Welch t-test: Age by Violence ---\n")
  cat("Mean age (violence = yes):", mean_yes, "\n")
  cat("Mean age (violence = no):",  mean_no,  "\n")
  cat("t =", round(test$statistic, 3),
      "| df =", round(test$parameter, 1), "\n")
  cat("p-value:", round(p_val, 4), "\n")
  if (sig) {
    cat("Result: p-value <", alpha, "\u2234 statistically significant.\n")
    cat("Conclusion: Sufficient evidence to reject H0 (mean ages differ).\n")
  } else {
    cat("Result: p-value >=", alpha, "\u2234 not statistically significant.\n")
    cat("Conclusion: Fail to reject H0 (no evidence of difference in mean ages).\n")
  }
  
  data.frame(
    Test        = "Welch t-test",
    Variable    = "Age",
    Mean_Yes    = mean_yes,
    Mean_No     = mean_no,
    t_statistic = round(test$statistic, 3),
    df          = round(test$parameter, 1),
    P_Value     = round(p_val, 4),
    Significant = ifelse(sig, "Yes ***", "No")
  )
}

ttest_result <- report_ttest_age(dv)

save_table_as_plot(
  df       = ttest_result,
  title    = "Welch t-test: Age by Violence Outcome",
  filename = "outputs/tables/05_ttest_age.png",
  width    = 9
)


# --- 4.3. Wilcoxon Rank-Sum Test (Income) ---
# Income is heavily right-skewed with many zeros -> non-parametric test preferred
report_wilcox_income <- function(data, alpha = 0.05) {
  test  <- wilcox.test(income ~ violence, data = data)
  p_val <- test$p.value
  sig   <- p_val < alpha
  
  med_yes <- round(median(data$income[data$violence == "yes"], na.rm = TRUE), 2)
  med_no  <- round(median(data$income[data$violence == "no"],  na.rm = TRUE), 2)
  
  cat("\n--- Wilcoxon rank-sum: Income by Violence ---\n")
  cat("Median income (violence = yes):", med_yes, "\n")
  cat("Median income (violence = no):",  med_no,  "\n")
  cat("W =", round(test$statistic, 3), "\n")
  cat("p-value:", round(p_val, 4), "\n")
  if (sig) {
    cat("Result: p-value <", alpha, "\u2234 statistically significant.\n")
    cat("Conclusion: Sufficient evidence to reject H0 (income distributions differ).\n")
  } else {
    cat("Result: p-value >=", alpha, "\u2234 not statistically significant.\n")
    cat("Conclusion: Fail to reject H0 (no evidence of difference in income distributions).\n")
  }
  
  data.frame(
    Test        = "Wilcoxon rank-sum",
    Variable    = "Income",
    Median_Yes  = med_yes,
    Median_No   = med_no,
    W_statistic = round(test$statistic, 3),
    P_Value     = round(p_val, 4),
    Significant = ifelse(sig, "Yes ***", "No")
  )
}

wilcox_result <- report_wilcox_income(dv)

save_table_as_plot(
  df       = wilcox_result,
  title    = "Wilcoxon Rank-Sum Test: Income by Violence Outcome",
  filename = "outputs/tables/06_wilcox_income.png",
  width    = 9
)


# =============================================================================
# SECTION 5: LOGISTIC REGRESSION
# =============================================================================

# --- 5.1. Full Multivariable Logistic Regression Model ---
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

model_summary_df <- broom::tidy(model) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

save_table_as_plot(
  df       = as.data.frame(model_summary_df),
  title    = "Logistic Regression — Model Summary",
  filename = "outputs/tables/07_model_summary.png",
  width    = 10
)


# --- 5.2. Correlation Matrix (Exploratory Mutlicollinearity Check) ---
# Use model variables only
dv_corr <- dv_model %>%
  select(age, income_log, education, employment, marital_status, violence_bin)

# Convert factors to numeric codes for exploratory correlation only
dv_corr_num <- dv_corr %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

corr_matrix <- cor(dv_corr_num, use = "complete.obs")

# Reshape for plotting
corr_long <- melt(corr_matrix)

corr_plot <- ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low      = "#00ff00",
    mid      = "white",
    high     = "#ff1493",
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "Correlation"
  ) +
  labs(
    title = "Correlation Matrix of Model Variables",
    subtitle = "Exploratory check for potential multicollinearity",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
ggsave("outputs/plots/11_correlation_matrix.png", width = 7, height = 6, dpi = 150)


# --- 5.3. Multicollinearity Check (GVIF) ---
gvif_table <- car::vif(model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Predictor") %>%
  transmute(
    Predictor = Predictor,
    GVIF      = round(GVIF, 3),
    Df        = Df,
    Adj_GVIF  = round(GVIF^(1/(2*Df)), 3)
  )

gvif_interpretation <- gvif_table %>%
  mutate(
    Interpretation = case_when(
      Adj_GVIF < 1.5 ~ "No multicollinearity concern",
      Adj_GVIF < 2   ~ "Low multicollinearity (acceptable)",
      Adj_GVIF < 5   ~ "Moderate collinearity (review recommended)",
      TRUE           ~ "High multicollinearity (potential instability)"
    )
  )

write_csv(gvif_interpretation, "outputs/gvif_interpretation.csv")
cat("\n--- Adjusted Variance Inflation Factor (GVIF) ---\n")
print(gvif_interpretation, row.names = FALSE)

save_table_as_plot(
  df       = gvif_interpretation,
  title    = "Multicollinearity Check (GVIF)",
  filename = "outputs/tables/08_gvif_interpretation.png",
  width    = 9
)


# --- 5.4. Odds Ratios Tables + Confidence Intervals ---
or_table <- model %>%
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  transmute(
    Predictor   = term,
    Odds_Ratio  = round(estimate, 3),
    Std_Error   = round(std.error, 3),
    Statistic   = round(statistic, 3),
    P_Value     = round(p.value, 4),
    CI_Lower    = round(conf.low, 3),
    CI_Upper    = round(conf.high, 3)
  )
  
cat("\n--- Logistic Regression Results (Odds Ratios) ---\n")
print(or_table, row.names = FALSE)

save_table_as_plot(
  df       = or_table,
  title    = "Logistic Regression — Odds Ratios (Income log-transformed) with 95% Confidence Intervals",
  filename = "outputs/tables/09_or_table.png",
  width    = 10
)


# --- 5.5. Forest Plot of Odds Ratios ---
# Cap CI bounds to avoid axis explosion from near-perfect separation
# (e.g. "employmentemployed" has only 3 cases — model coefficients are unstable)
or_plot <- or_table %>%
  mutate(
    CI_Lower_capped  = pmax(CI_Lower, 0),
    CI_Upper_capped  = pmin(CI_Upper, 10),
    OR_capped        = pmin(Odds_Ratio, 10)   # <-- cap the point too
  )

ggplot(or_plot, aes(x = OR_capped, y = reorder(Predictor, OR_capped))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(
    aes(xmin = CI_Lower_capped, xmax = CI_Upper_capped),
    height = 0.25,
    colour = "grey40"
  ) +
  geom_point(
    aes(colour = P_Value < 0.05),
    size = 3
  ) +
  scale_colour_manual(
    values = c("TRUE" = "#ff1493", "FALSE" = "#00ff00"),
    name   = "p < 0.05"
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.0001),
    limits = c(0, 10)
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0.5, size = 10, colour = "grey40")
  ) +
  labs(
    title    = "Logistic Regression: Odds Ratios",
    subtitle = "95% CI shown; income log-transformed due to skewness;\npink = statistically significant at α = 0.05\nNote: OR and CIs capped at 10",
    x        = "Odds Ratio (capped at 10)",
    y        = NULL,
    caption  = "--- Dashed line marks OR = 1 (no effect)" 
  )
ggsave("outputs/plots/12_odds_ratios.png", width = 9, height = 6, dpi = 150)


# =============================================================================
# SECTION 6: MODEL DIAGNOSTICS
# =============================================================================

# --- 6.1. Predicted Probabilities ---
# Threshold is adjustable
threshold <- 0.5

# Add predicted probabilities and binary predictions to the modelling data
dv_model <- dv_model %>%
  mutate(
    pred_prob  = predict(model, type = "response"),   # P(violence = yes)
    pred_class = if_else(pred_prob >= threshold, 1L, 0L)
  )

# Distribution of predicted probabilities by actual outcome
ggplot(dv_model, aes(x = pred_prob, fill = violence)) +
  geom_histogram(binwidth = 0.05, colour = "white", alpha = 0.75,
                 position = "identity") +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = violence_colours, name = "Actual outcome") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(
    title    = "Distribution of Predicted Probabilities",
    subtitle = "Dashed line marks the 0.5 classification threshold",
    x        = "Predicted probability of violence",
    y        = "Count"
  )
ggsave("outputs/plots/13_predicted_probabilities.png", width = 8, height = 5, dpi = 150)


# --- 6.2. Confusion Matrix at 0.5 Threshold ---
# Build the confusion matrix as a tidy data frame
conf_matrix <- dv_model %>%
  count(Actual    = factor(violence_bin, levels = c(0, 1),
                           labels = c("No", "Yes")),
        Predicted = factor(pred_class,   levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  group_by(Actual) %>%
  mutate(
    row_total = sum(n),
    pct       = round(n / row_total * 100, 1)
  ) %>%
  ungroup()

# Tile plot (heatmap-style confusion matrix)
ggplot(conf_matrix,
       aes(x = Predicted, y = Actual, fill = n)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(
    aes(label = paste0(n, "\n(", pct, "%)")),
    size     = 5,
    fontface = "bold",
    colour   = "#111111"
  ) +
  scale_fill_gradient(
    low   = "#00ff00", 
    high  = "#ff1493",
    name  = "Count",
    guide = guide_colourbar(
      barwidth       = 10,
      barheight      = 0.8,
      title.position = "left",
      title.vjust    = 0.8
    )
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Confusion Matrix (Threshold = 0.5)",
    subtitle = "Percentages are row-wise (within each actual class)",
    x        = "Predicted",
    y        = "Actual"
  ) +
  theme(
    panel.grid   = element_blank(),
    axis.text    = element_text(size = 12, face = "bold"),
    axis.title   = element_text(size = 12),
    legend.text  = element_text(angle = 45, hjust = 1)
  )
ggsave("outputs/plots/14_confusion_matrix.png", width = 6, height = 5, dpi = 150)


# --- 6.3 Classification Metrics ---
metrics <- dv_model %>%
  mutate(
    Actual    = factor(violence_bin, levels = c(0, 1), labels = c("No", "Yes")),
    Predicted = factor(pred_class,   levels = c(0, 1), labels = c("No", "Yes"))
  )
metrics_vec <- c(
  Accuracy    = yardstick::accuracy_vec(metrics$Actual, metrics$Predicted),
  Sensitivity = yardstick::sens_vec(metrics$Actual, metrics$Predicted),
  Specificity = yardstick::spec_vec(metrics$Actual, metrics$Predicted),
  Precision   = yardstick::precision_vec(metrics$Actual, metrics$Predicted)
)

metrics_table <- data.frame(
  Metric = names(metrics_vec),
  Value_Pct = paste0(round(metrics_vec * 100, 1), "%"),
  Description = c(
    "Overall correct classifications",
    "Of actual YES, correctly predicted",
    "Of actual NO, correctly predicted",
    "Of predicted YES, actually YES"
  )
)

save_table_as_plot(
  df       = metrics_table,
  title    = "Classification Metrics (Threshold = 0.5)",
  filename = "outputs/tables/10_classification_metrics.png",
  width    = 9
)

# --- 6.4. Calibration Plot ---
calibration_df <- dv_model %>%
  mutate(
    Actual = violence_bin
  ) %>%
  mutate(
    bin = ntile(pred_prob, 10)   # 10 bins (deciles)
  ) %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(pred_prob),
    obs_rate  = mean(Actual),
    n         = n()
  )

ggplot(calibration_df, aes(x = mean_pred, y = obs_rate)) +
  geom_point(aes(size = n), colour = "#ff1493", alpha = 0.8) +
  scale_size(range = c(2, 6), name = "Bin size") +
  geom_line(colour = "#ff1493", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey40") +
  labs(
    title    = "Calibration Plot",
    subtitle = "Observed vs Predicted Probability of Violence",
    x        = "Mean Predicted Probability",
    y        = "Observed Proportion (Actual Violence)"
  ) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave("outputs/plots/15_calibration_plot.png", width = 6, height = 5, dpi = 150)


# =============================================================================
# SECTION 7: SUMMARY
# =============================================================================
summary_text <- c(
  "--- Final Summary of Findings ---",
  
  "1. Key demographic patterns:",
  "- The dataset is imbalanced, with more non-violent than violent cases.",
  "- Income is highly right-skewed and heavily zero-inflated, with over half of respondents reporting zero income.",
  "- To address this, income was log-transformed (log1p), improving model stability and interpretability.",
  "- Median income is 0 in both groups, indicating that at least 50% of participants have no reported income.",
  "- The proportion of zero income is higher in the violence group, suggesting a potential socioeconomic gradient.",
  "- Clear visual differences exist across education, employment, and marital status.",
  
  "2. Statistical inference:",
  "- Education, employment, and marital status are significantly associated with violence (Chi-square tests).",
  "- Age shows weaker evidence of association (Welch t-test).",
  "- Income differs significantly between groups (Wilcoxon test), consistent with its skewed distribution.",
  
  "3. Multicollinearity assessment:",
  "- The correlation matrix shows no strong linear relationships between predictors.",
  "- Adjusted GVIF values indicate no problematic multicollinearity.",
  "- This supports the stability and interpretability of the regression coefficients.",
  
  "4. Logistic regression findings:",
  "- Socioeconomic variables remain important after adjustment.",
  "- Employment and marital status emerge as the strongest predictors.",
  "- Higher (log-transformed) income is associated with reduced likelihood of experiencing violence.",
  "- Model estimates are stable and not adversely affected by multicollinearity.",
  
  "5. Model performance:",
  "- The model predicts non-violence more accurately than violence.",
  "- Sensitivity is lower than specificity, reflecting class imbalance.",
  "- Accuracy is inflated due to the higher proportion of non-violent cases.",
  
  "6. Calibration and probability assessment:",
  "- Calibration analysis compares predicted probabilities with observed outcomes across groups.",
  "- Predictions were grouped into bins, and the average predicted probability was compared with the actual observed rate.",
  "- Points close to the 45-degree line indicate well-calibrated predictions.",
  "- Deviations from this line suggest over- or under-estimation of risk in certain probability ranges.",
  "- Larger bins provide more reliable estimates of model calibration.",
  
  "7. Conclusion:",
  "- Domestic violence is strongly associated with socioeconomic factors.",
  "- Employment and marital status are the most consistent predictors.",
  "- Income shows a meaningful but complex relationship due to extreme zero inflation.",
  "- Overall, the model demonstrates reasonable predictive performance and good calibration, with no major multicollinearity concerns."
)

cat(paste(summary_text, collapse = "\n"))
writeLines(summary_text, "outputs/final_summary.txt")