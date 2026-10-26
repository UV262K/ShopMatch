# ShopMatch — Product Recommendation System

ShopMatch is a college project built in R and RStudio. I used transaction data from an online retailer to look at sales patterns, customer spending, and whether previous purchases could help suggest other products.

## What is included

- Data cleaning and preparation
- Exploratory charts for revenue, product sales, basket size, and customer spend
- Statistical tests comparing customer groups
- Logistic regression to predict whether a customer buys a popular product
- Linear regression to estimate customer spending
- An item-based collaborative filtering recommender using cosine similarity
- Evaluation against popularity and random recommendation baselines

## Tools

R, RStudio, dplyr, tidyr, ggplot2, readr, readxl, and scales.

## Dataset

This project uses the UCI Online Retail dataset: https://archive.ics.uci.edu/dataset/352/online+retail

Download the Excel dataset separately and put `Online Retail.xlsx` in the same folder as `shopmatch.R`. The script can also read a compatible `data.csv`. If it cannot load the real data, it falls back to simulated data and prints a warning, so check the console output before interpreting results.

## How to run

1. Install R and RStudio.
2. Download the dataset and place it next to `shopmatch.R`.
3. Open `shopmatch.R` in RStudio.
4. Set the working directory to the script's folder.
5. Click **Source**. The script creates an `outputs/` folder for charts and results.
6. To rebuild the report, open `ShopMatch_Report.Rmd` and knit it in RStudio.

The script installs required R packages if they are missing.

## Results from the accompanying report

- Logistic regression ROC AUC: **0.734**
- Linear regression test R-squared: **0.704**
- Item-based collaborative filtering HitRate@10: **44.2%**
- Popularity baseline HitRate@10: **21.1%**

These are the values recorded in the provided report and may differ if the analysis is run again with different data or settings.

## Limitations

The recommender uses purchase history rather than explicit ratings, has limited information for new customers, and only models the 100 most popular products.

## Contributors

- Yuvraj
- Sagar Raut

No license has been added yet.
