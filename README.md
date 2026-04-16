# 📊 Domestic Violence Analysis

> **An R-based statistical analysis examining socioeconomic and demographic risk factors associated with domestic violence against women.**

---

## Overview

This end-to-end project investigates how factors such as **age**,**education**, **employment**, **income**, and **marital status** relate to women's vulnerability to domestic violence. Using survey-based data, the analysis moves from exploratory investigation through to formal statistical modelling, with the goal of identifying meaningful patterns and predictors of vulnerability.

This repository forms part of an ongoing data analytics portfolio. A full written report (PDF) accompanies this repository with detailed methodology, findings, and conclusion.

---

## Research Focus

The analysis centres on the following key questions:

- How do socioeconomic factors (education level, employment status, income) relate to domestic violence vulnerability?
- Does marital status influence exposure to or experience of domestic violence?
- Which demographic and contextual factors are most predictive of vulnerability?

---

## Methods

| Stage | Description |
|---|---|
| **Exploratory Data Analysis (EDA)** | Distribution analysis, summary statistics, missingness checks, and initial variable relationships |
| **Data Visualisation** | Charts and plots to communicate patterns across demographic groups |
| **Statistical Modelling** | Regression-based approaches to model vulnerability outcomes |
| **Statistical Inference** | Hypothesis testing and interpretation of results in context |

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
- **Output formats:** HTML (rendered analysis outputs); visualisations produced in R using `ggplot2`

Key R packages used include `ggplot2`, `dplyr`, `tidyr`, and base R statistical functions.

---

## Data

The dataset is sourced from [Kaggle](https://www.kaggle.com/datasets/fahmidachowdhury/domestic-violence-against-women) 
and contains survey responses capturing women's experiences and socioeconomic 
circumstances. Variables include education level, employment and income status, 
marital status, and indicators of exposure to domestic violence.

> **Note:** Raw data files are stored in the `data/` directory. Refer to the full PDF report for data source details, variable definitions, and ethical considerations around this sensitive topic.

---

## Full Report

A comprehensive written report (PDF) accompanies this repository and covers:

- Detailed methodology and variable selection rationale
- Full results with interpretation
- Limitations and ethical considerations
- Conclusions and recommendations

---

## About

This project is part of a data analytics portfolio focused on applying statistical and data science methods to real-world social issues. 
The analysis is conducted with care for the sensitivity of the subject matter, recognising that domestic violence data involves real experiences of harm.

---

*Built with R · Part of an ongoing analytics portfolio*
