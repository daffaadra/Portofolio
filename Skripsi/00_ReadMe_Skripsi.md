# Thesis (Skripsi)

### Overview
These two codes are part of my undergraduate thesis titled "Analysis of Spotify's Audio Features Trends using Time Series Decomposition and Vector Autoregressive (VAR) Model".
- **Thesis Objective**: Analyze musical trends on the streaming platform since the beginning of 2017 until mid 2023 from the 200 most popular songs (based on streams) weekly.
- **Data Collection**: Spotify's official chart website and the platform's Developer API.

### Steps
- This project is divided into two parts:
  1. Time series decomposition of the data using Seasonal/Classical Decomposition and Seasonal-Trend Decomposition Based on Loess (Locally Estimated Regression).
  2. Analysis using Vector Autoregressive (VAR) model. This part covers building the model, estimating the parameters, and forecasting future values. 
- *"Plot + Dekomposisi.ipnyb"* file shows the basic visualization of the data along with the time series decomposition process. The decomposed data was extracted and will be used for further analysis with VAR model.
- *"VAR.ipnyb"* file demonstrates building and analyzing VAR model chronologically. The steps involved are:
  1. Granger Causality test
  2. Stationary (Augmented Dicky-Fuller) test
  3. Cointegration (Johansen) test
  4. Model selection (order of the model) using AIC, BIC, and HQIC
  5. Parameter estimation
  6. Forecasting & Error analysis
