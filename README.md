# time-series-project
Time series modeling project forecasting hourly energy usage data 168 hours out. 


Data cleaning and manipulation:

Transform data into eastern time zone, and deal with daylight savings time which created gaps and missing data (these missing 
data points were imputed using last observation carried forward)

Additionally external temperature data was brought in to assist with forecast 

then transform data into a tsibble in order to model time series 



Mdeling techniques

Many modeling techniques were attempted, these include:

ESM 
SARIMA 
Prophet (facebook's model)
TBATS
Neural Network
XGBoost 

the final model used was an ensemble of the XGBoost and Prophet models 


Accuracy and visualization:

MAPE and MAE were the main forms of accuracy assessment 

the final model visualized the predictions plotted against the test data set to visually represent accuracy 


