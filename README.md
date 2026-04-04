# Financial Asset Valuation in R

This project was carried out as part of the **Financial Asset Analysis** course in the **M1 ECAP program**.

Its objective was to analyze and compare the performance of **10 financial assets**, including **9 publicly listed football clubs** and **Solana (SOL-USD)** as a benchmark/reference asset, using portfolio management and financial risk analysis methods in **R**.

## Project Overview

The analysis was based on historical market data collected from **Yahoo Finance** over the period **01/12/2023 to 30/11/2024**.

The project focused on:
- price evolution analysis
- return calculation
- descriptive statistical analysis of returns
- risk and volatility assessment
- covariance and correlation analysis
- portfolio performance measurement
- portfolio construction and optimization

## Main Tasks Performed

### Data Preparation
- Imported and structured data from multiple Excel files
- Harmonized column names across all datasets
- Merged several financial time series into a single working dataset
- Cleaned missing values and prepared return series for analysis

### Exploratory Data Analysis
- Visualized asset price movements over time
- Visualized return dynamics for each asset
- Created boxplots and histograms of returns
- Built normalized price indices to compare asset trajectories

### Statistical Analysis
- Computed:
  - mean return
  - variance
  - standard deviation
  - skewness
  - kurtosis
- Estimated covariance and correlation matrices between assets

### Performance and Risk Metrics
The project included several portfolio and asset performance indicators such as:
- Sharpe Ratio
- Sortino Ratio
- Treynor Ratio
- Jensen’s Alpha
- Coefficient of Variation
- Information Ratio

### Portfolio Analysis
- Constructed an equally weighted portfolio
- Compared assets in a **risk-return framework**
- Built an **efficient frontier** for selected assets
- Calculated:
  - minimum variance portfolio
  - tangent portfolio

## Tools and Technologies
- **R**
- **data.table**
- **dplyr**
- **ggplot2**
- **PerformanceAnalytics**
- **FactoMineR**
- **corrplot**
- **readxl**
- **xts**
- **fPortfolio**

## Key Skills Demonstrated
- Financial data cleaning and transformation
- Time series manipulation in R
- Statistical analysis of asset returns
- Risk and performance evaluation
- Data visualization
- Portfolio construction and optimization

## Repository Structure

- `Code_du_Projet.R` — main R script containing the full analysis
- `README.md` — project presentation
- `Evaluation_Actifs_Financiers.Rproj` — R project file

## Academic Report

The full academic report is available in **French**.

**Access the full report (French):** [Click here](YOUR_LINK_HERE)

## Note

Although the detailed academic report is written in French, this repository provides an **English overview** of the project, methodology, and main techniques used.
