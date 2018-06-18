# Background ----
#Data Sources
  #Common data sources for predictive maintenance problems are
  #Failure history: The failure history of a machine or component within the machine. 
  #Maintenance history: The repair history of a machine, e.g. error codes, previous maintenance activities or component replacements. 
  #Machine conditions and usage: The operating conditions of a machine e.g. data collected from sensors. 
  #Machine features: The features of a machine, e.g. engine size, make and model, location. 
  #Operator features: The features of the operator, e.g. gender, past experience 
#The data for this example comes from 4 different sources which are real-time telemetry data collected from machines, error messages, historical maintenance records that include failures and machine information such as type and age.


# Load required libraries ----
library("AzureML")
library("dplyr") # Data munging functions
library("zoo")   # Feature engineering rolling aggregates
library("data.table") # Feature engineering
library("ggplot2") # Graphics
library("scales") # For time formatted axis
options(stringsAsFactors = FALSE)


# Load required dataset and exploratory analysis ----
# connect to the workspace and pull datasets that exist
# ws <- workspace('0d06a61e9ff143939b5f286287128b76', 'lKFnCR5tG+cE8aBQKmf07hihm3qjIiCjPfe7Zdo76Sz6WbHkO5caNAlWEAVjwt8c5lySJGp8x38wgjYRSKqKRw==')
# download telemetry dataset
# telemetry <- download.datasets(ws, name = "telemetry")
# Instead pull from CSV file present already
# set working folders 
setwd("C:/Kamal/Work/20_Kaggle/Pred_Maint")


# Analysis on TELEMETRY data ----
telemetry <- read.csv("./Data/PdM_telemetry.csv", header = T, na.strings = c(""," ","NA"))
# format datetime field which comes in as.character
telemetry$datetime <- as.POSIXct(telemetry$datetime,
                                 format="%Y-%m-%d %I:%M:%S",
                                 tz="UTC")
cat("Total Number of telemetry records:", nrow(telemetry))
range(telemetry$datetime)
head(telemetry,10)
tail(telemetry,10)
summary(telemetry)
# As an example, below is a plot of voltage values for two machineIDs for January 2015.
theme_set(theme_bw())  # theme for figures
options(repr.plot.width = 8, repr.plot.height = 6)
ggplot(data = telemetry %>% filter(machineID %in% 1:2, 
                                   datetime > as.POSIXct("2015-01-01"),
                                   datetime < as.POSIXct("2015-02-01")),
       aes(x = datetime, y = volt, col = factor(machineID))) +
  geom_line(alpha = 0.5) +
  labs(y = "voltage", color = "machineID") +
  facet_wrap(~machineID, ncol=1)



# Analysis on ERRORS data ----
errors <- read.csv("./Data/PdM_errors.csv", header = T, na.strings = c(""," ","NA"))
# format datetime and errorID fields
errors$datetime <- as.POSIXct(errors$datetime,
                              format="%Y-%m-%d %I:%M:%S", 
                              tz="UTC")
errors$errorID <- as.factor(errors$errorID)
cat("Total Number of error records:",nrow(errors))
errors[c(1:5, nrow(errors)-4:1),]

options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(errors, aes(x = errorID)) + 
  geom_histogram(fill = "orange",stat="count") + 
  labs(title = "Errors by type", x = "error types")

options(repr.plot.width = 6, repr.plot.height = 5)
ggplot(errors %>% filter(machineID < 4), 
       aes(x = errorID, fill = factor(machineID))) + 
  geom_histogram(color = "black",stat="count") + 
  labs(title = "MachineID errors by type", x = "error types", fill="MachineID")+
  facet_wrap(~machineID, ncol = 1)

options(repr.plot.width = 7, repr.plot.height = 5)
ggplot(errors %>% filter(machineID == 4), 
       aes(y = errorID, x = datetime)) + 
  geom_point(color = "black", alpha = 0.5) + 
  labs(title = "MachineID 4 errors", x = "Date")


# Analysis on MAINTENANCE data ----
maint <- read.csv("./Data/PdM_maint.csv", header = T, na.strings = c(""," ","NA"))
# These are the scheduled and unscheduled maintenance records which correspond to both regular inspection of components 
# as well as failures. A record is generated if a component is replaced during the scheduled inspection or replaced due 
# to a break down. The records that are created due to break downs will be called failures which is explained in the 
# later sections. Maintenance data has both 2014 and 2015 records.

# format datetime and comp fields
maint$datetime <- as.POSIXct(maint$datetime,
                             format="%Y-%m-%d %I:%M:%S", 
                             tz="UTC")
maint$comp <- as.factor(maint$comp)
invis
cat("Total number of maintenance records:", nrow(maint))
range(maint$datetime)
maint[c(1:5, nrow(maint)-4:0),]

options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(maint, aes(x = comp)) + 
  geom_histogram(fill= "magenta",stat="count") +
  labs(title = "Component replacements", x = "component types")

options(repr.plot.width = 6, repr.plot.height = 8)
ggplot(maint %>% filter(machineID < 4), 
       aes(x = comp, fill = factor(machineID))) + 
  geom_histogram(color = "black",stat="count") +
  labs(title = "Component replacements", x = "component types", fill = "Machine ID")+
  facet_wrap(~machineID, ncol = 1)

options(repr.plot.width = 7, repr.plot.height = 5)
ggplot(maint %>% filter(machineID == 4), 
       aes(y = comp, x = datetime)) + 
  geom_point(color = "black", alpha = 0.5) + 
  labs(title = "MachineID 4 component replacements", x = "Date")


# Analysis on MACHINES data ----
machines <- read.csv("./Data/PdM_machines.csv", header = T, na.strings = c(""," ","NA"))
# format model field
machines$model <- as.factor(machines$model)
cat("Total number of machines:", nrow(machines))
machines[c(1:5, nrow(machines)-4:0),]
summary(machines)

options(repr.plot.width = 8, repr.plot.height = 6)
ggplot(machines, aes(x = age, fill = model)) + 
  geom_histogram(color = "black",stat="count") + 
  labs(title = "Machines", x = "age (years)") +
  facet_wrap(~model)



# Analysis on FAILURES data ----
failures <- read.csv("./Data/PdM_failures.csv", header = T, na.strings = c(""," ","NA"))

# format datetime and failure fields
failures$datetime <- as.POSIXct(failures$datetime,
                                format="%Y-%m-%d %I:%M:%S", 
                                tz="UTC")
failures$failure <- as.factor(failures$failure)

cat("Total number of failures:", nrow(failures))
failures[c(1:5, nrow(failures)-4:0),]

options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(failures, aes(x = failure)) + 
  geom_histogram(fill = "red",stat="count") + 
  labs(title = "Failure distribution", x = "component type")

options(repr.plot.width = 6, repr.plot.height = 6)
ggplot(failures %>% filter(machineID < 4),
       aes(x = failure, fill = factor(machineID))) + 
  geom_histogram(color = "black",stat="count") + 
  labs(title = "Failure distribution", x = "component type", fill = "MachineID") +
  facet_wrap(~machineID, ncol=1)
  
invisible(gc())

