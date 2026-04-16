# 📊 Domestic Violence Analysis

> **An R-based statistical analysis examining socioeconomic and demographic risk factors associated with domestic violence against women.**

---

## Overview

This end-to-end project investigates how factors such as **age**,**education**, **employment**, **income**, and **marital status** relate to women's vulnerability to domestic violence. Using survey-based data of 347 observations, the analysis moves from exploratory investigation through to formal statistical modelling, with the goal of identifying meaningful patterns and predictors of vulnerability.

This repository forms part of an ongoing data analytics portfolio. A full written report (PDF) accompanies this repository with detailed methodology, findings, and conclusion.

---

## Research Focus

The analysis centres on the following key questions:

- How do socioeconomic factors (education level, employment status, income) relate to domestic violence vulnerability?
- Does marital status influence exposure to or experience of domestic violence?
- Which demographic and contextual factors are most predictive of vulnerability?

---

## Dataset

The dataset is sourced from [Kaggle](https://www.kaggle.com/datasets/fahmidachowdhury/domestic-violence-against-women) and contains 347 survey observations with the following variables:

| Variable | Type | Description |
|---|---|---|
| `age` | Continuous | Age of the respondent |
| `education` | Ordinal | None / Primary / Secondary / Tertiary |
| `employment` | Ordinal | Unemployed / Semi-employed / Employed |
| `income` | Continuous | Reported income (heavily zero-inflated) |
| `marital_status` | Categorical | Married / Unmarried |
| `violence` | Binary (outcome) | Whether the respondent experienced domestic violence |

> **Note:** Income is heavily right-skewed and zero-inflated. It is log-transformed (`log1p`) throughout modelling to improve stability and interpretability.

---

## Methods

### Section 1 — Setup
Custom ggplot2 theme and colour palette applied consistently across all visualisations. A helper function automates saving summary tables as styled PNG outputs.

### Section 2 — Data Import & Cleaning
- Raw CSV import and inspection (`glimpse`, `summary`, `head`)
- Missing value check across all columns
- Column renaming and whitespace stripping
- Typo correction (e.g. `"unmarred"` → `"unmarried"`)
- Factor encoding with logical level ordering for all categorical variables

### Section 3 — Exploratory Data Analysis (EDA)
Visualisations produced in R using `ggplot2`:

| Plot | Description |
|---|---|
| Outcome distribution | Bar chart of violence vs non-violence (with percentages) |
| Age distribution | Overlapping histograms by violence outcome |
| Education vs violence | Stacked bar chart and lollipop chart of violence rates |
| Employment vs violence | Line graph of violence proportion by employment status |
| Income distribution | Density plot (log1p scale) and QQ plot for normality check |
| Income across education | Ridge density plot by education level and violence outcome |
| Marital status vs violence | Stacked bar chart |
| Age vs income | Scatter plot coloured by violence outcome |

### Section 4 — Statistical Inference

| Test | Variable(s) | Rationale |
|---|---|---|
| Chi-square test | Education, Employment, Marital Status | Tests association between categorical predictors and violence outcome |
| Welch t-test | Age | Compares mean age between violence groups (unequal variances assumed) |
| Wilcoxon rank-sum test | Income | Non-parametric test used due to severe skewness and zero inflation |

All tests use α = 0.05. Results are saved as styled table outputs.

### Section 5 — Logistic Regression
- **Full multivariable logistic regression** with all five predictors
- **Correlation matrix** for exploratory multicollinearity assessment
- **GVIF (Generalised Variance Inflation Factor)** for formal multicollinearity testing, including adjusted GVIF and interpretation
- **Odds ratio table** with 95% confidence intervals
- **Forest plot** of odds ratios (CIs capped at 10 to handle near-separation instability in sparse groups)

### Section 6 — Model Diagnostics
- **Predicted probability distribution** histogram by actual outcome
- **Confusion matrix** heatmap at 0.5 classification threshold
- **Classification metrics**: Accuracy, Sensitivity, Specificity, Precision
- **Calibration plot**: Observed vs predicted probability across decile bins

### Section 7 — Summary
Key findings printed to console and saved as a plain text report.

---

## Key Findings

- The dataset is imbalanced (~3:1 non-violent to violent cases)
- **Education, employment, and marital status** are significantly associated with violence (Chi-square tests)
- **Employment and marital status** are the strongest predictors in the logistic regression model
- **Higher income** (log-transformed) is associated with reduced likelihood of violence
- No problematic multicollinearity detected (adjusted GVIF values are within acceptable range)
- The model predicts non-violence more accurately than violence, reflecting class imbalance

---

## Repository Structure

```
Domestic-Violence-Analysis/
│
├── data/                  # Raw dataset
│
├── scripts/               # R scripts for analysis, html file for summary report
│
├── outputs/               # Generated plots, tables, and results
│
├── Domestic-Violence-Analysis.Rproj   # RStudio project file
├── .gitignore
└── README.md
```

---

## Tech Stack

- **Language:** R
- **Environment:** RStudio (`.Rproj`)
- **Output formats:** HTML (rendered analysis outputs); PNG visualisations produced in R using `ggplot2`
- **Key Packages:** `tidyverse`, `ggplot2`, `ggridges`, `car`, `broom`, `yardstick`, `gridExtra`, `reshape2`

---

## Full Report

A comprehensive written report (PDF) accompanies this repository and covers A comprehensive written report (PDF) accompanies this repository and covers detailed methodology, full results with interpretation, limitations, ethical considerations, and conclusions.

---

## About

This project is part of a data analytics portfolio focused on applying statistical and data science methods to real-world social issues. 
The analysis is conducted with care for the sensitivity of the subject matter, recognising that domestic violence data involves real experiences of harm.

---

*Built with R · Part of an ongoing analytics portfolio*
