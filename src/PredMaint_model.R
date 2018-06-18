
# set working folders 
setwd("C:/Kamal/Work/20_Kaggle/Pred_Maint")

# reference source code needed ----
source("./PredMaint_EDA.R")

# Feature Engineering ----
# The first step in predictive maintenance applications is feature engineering which requires bringing 
# the different data sources together to create features that best describe a machines's health condition 
# at a given point in time. In the next sections, different type of feature engineering methods are used 
# to create features based on the properties of each data source.

# Lag Features from Telemetry
# Telemetry data almost always comes with time-stamps which makes it suitable for calculating lagging features. 
# A common method is to pick a window size for the lag features to be created and compute rolling aggregate measures 
# such as mean, standard deviation, minimum, maximum, etc. to represent the short term history of the telemetry 
# over the lag window. In the following, rolling mean and standard deviation of the telemetry data over the last 
# 3 hour lag window is calculated for every 3 hours.

# calculate the rolling mean and rolling standard deviation 
# on the last 3 hour lag window (width=3), for every 3 hours (by=3)
# for each machine ID.
telemetrymean <- telemetry %>%
  arrange(machineID, datetime) %>% 
  group_by(machineID) %>%
  mutate(voltmean = rollapply(volt, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean = rollapply(rotate, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean = rollapply(pressure, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean = rollapply(vibration, width = 3, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean, rotatemean, pressuremean, vibrationmean) %>% 
  ungroup()

head(telemetrymean)


