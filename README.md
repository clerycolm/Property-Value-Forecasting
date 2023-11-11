# Forecasting Residential Dwelling Property Transaction Value in Ireland

Report was Time Series Analysis module, taken during my final year at University College Cork.

This analysis delves into residential dwelling property transaction values in Ireland using a dataset from the Central Statistics Office. The objective is to explore the concerns surrounding the Republic of Ireland's housing crisis through a time series approach.

The data is divided into training and test sets. Initial examination reveals non-stationarity, prompting the creation of an integrated series of order 1. The chosen model is ARIMA(0,1,1)(1,0,1)12, selected after scrutinizing the Auto-correlation function (ACF) and Partial Auto-correlation function (PACF) through a hyperparameter search.

Model evaluation involves standardized residuals, autocorrelation functions, and Ljung-Box tests. The ARIMA(0,1,1)(1,0,1)12 model demonstrates randomness in residuals and independence across most lags, indicative of a good fit.

The forecast predicts a continued rise in mean household sale prices, with December 2024 projected at approximately €278,000—an alarming 96% increase from March 2012. 

Discussion notes the model's effectiveness in capturing the structural increase in sale prices but acknowledges room for improvement. Recommendations include exploring data transformations and considering the median instead of the mean to enhance robustness in tracking housing trends.

In conclusion, the analysis sheds light on Ireland's housing market dynamics, emphasizing a persistent crisis. The chosen model provides valuable insights, but its susceptibility to overfitting prompts a call for further refinement.

**Key Skills and Tools:** R, Time Series Analysis 
