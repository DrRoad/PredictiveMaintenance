
# PredictiveMaintenance

## The business problem - 
is about predicting problems caused by component failures such that the question “What is the probability that a machine will fail in the near future due to a failure of a certain component” can be answered. The problem is formatted as a multi-class classification problem and a machine learning algorithm is used to create the predictive model that learns from historical data collected from machines. 

## Data Sources

* Failure history: The failure history of a machine or component within the machine. 
* Maintenance history: The repair history of a machine, e.g. error codes, previous maintenance activities or component replacements. 
* Machine conditions and usage: The operating conditions of a machine e.g. data collected from sensors. 
* Machine features: The features of a machine, e.g. engine size, make and model, location. 
* Operator features: The features of the operator, e.g. gender, past experience 

The data for this example comes from 4 different sources which are real-time telemetry data collected from machines, error messages, historical maintenance records that include failures and machine information such as type and age.

