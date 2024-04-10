# Modeling Economic Impact on Personal Consumption Expenditures (PCE) #

## Overview ##
This project analyzes the influence of various economic indicators on PCE from Q1 2019 to Q4 2023. PCE is a crucial gauge of consumer spending and economic health.

## Data Sources ##
Data was compiled from:
- Federal Reserve Economic Data (FRED)
- Bureau of Economic Analysis (BEA)
- Federal Housing Finance Agency

## Data Description ##
Quarterly data includes:
- PCE by product type (78 products, 5 hierarchies)
- Real Disposable Personal Income
- Real Interest Rates
- 10-Year Breakeven Inflation Rate
- Consumer Sentiment
- Unemployment Rate
- House Price Index

## Data Processing ##
The dataset was cleansed and structured to reflect hierarchical categories, preparing it for a linear mixed-effects modeling approach.

## Model Insights ##
- The model demonstrates a positive trend in PCE over time, with inverse relationships to consumer sentiment and disposable income.
- Approximately 15% of PCE variation is explained by the model, indicating a moderate fit.
- Minimal variation in PCE is attributed to unobserved group-specific factors.
- The model is well-balanced in terms of fit and complexity.

## Further Research ##
Future studies could consider monthly data, additional variables, and nonlinear modeling to enhance understanding and prediction accuracy.

## References ##
Access the economic datasets used in this study:
- [BEA - Personal consumption expenditures](https://apps.bea.gov/iTable/?reqid=19&step=2&isuri=1&categories=survey)
- [FHFA - House Price Index Datasets](https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)
- [FRED - Economic Data](https://fred.stlouisfed.org/series)
