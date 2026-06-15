# Bayesian Network Imputation for Heart Disease Classification

**Authors:** Giuseppe Pianeti, Filippo Campagnolo  
**Course:** Advanced Machine Learning — Master in Statistical and Economic Methods for Decision Making, University of Turin  
**Academic Year:** 2025/2026

## Overview

This project investigates the use of Bayesian Networks (BN) for missing data imputation 
in a multi-clinic heart disease classification task. Rather than restricting the analysis 
to complete cases only, we leverage BN-based imputation to recover information from 
incomplete observations across four international clinics, and evaluate the impact of 
different imputation strategies on the performance of five classification models.

## Project Structure

```
├── data/                          # raw data files (not tracked by git)
├── results/                       # saved .rds objects for each scenario
├── data_loading_cleaning.R        # data loading, formatting and scenario setup
├── data_imputation.R              # BN structure learning, CV tuning, imputation
├── models_fitting.R               # model training and evaluation
├── data_storing.R                 # results collection and export
├── presentation.qmd               # Quarto presentation
├── report.qmd                     # Quarto report
├── report.pdf                     # PDF report
└── README.md
```

## How to Reproduce

1. Clone the repository  

2. Download the raw data from the 
[UCI Repository](https://archive.ics.uci.edu/dataset/45/heart+disease) 
and place the four `.data` files in the `data/` folder.

3. Install the required R packages:
```r
   install.packages(c("bnlearn", "Rgraphviz", "gRbase", "randomForest",
                      "dbarts", "tidyverse", "foreach", "doParallel",
                      "doRNG", "knitr", "kableExtra", "here"))
```

4. Run the scripts **in order**, setting the `scenario` variable at the top 
   of each script (0 through 4):
```
   data_loading_cleaning.R  →  data_imputation.R  →  
   models_fitting.R  →  data_storing.R
```

5. Render the report and presentation