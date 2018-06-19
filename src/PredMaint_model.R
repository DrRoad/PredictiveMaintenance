
# Set working folders and Reference source code needed ----
setwd("C:/Kamal/Work/20_Kaggle/Pred_Maint")
source("./PredMaint_EDA.R")

# Feature Engineering ----

# The first step in predictive maintenance applications is feature engineering which requires bringing 
# the different data sources together to create features that best describe a machines's health condition 
# at a given point in time. In the next sections, different type of feature engineering methods are used 
# to create features based on the properties of each data source.

# Lag Features from Telemetry ----

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

telemetrysd <- telemetry %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd = rollapply(volt, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd = rollapply(rotate, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd = rollapply(pressure, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd = rollapply(vibration, width = 3, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd, rotatesd, pressuresd, vibrationsd) %>%
  filter(!is.na(voltsd)) %>%
  ungroup()

head(telemetrysd)

# calculate the rolling mean and rolling standard deviation 
# on the last 24 hour lag window (width=24), for every 3 hours (by=3)
# for each machine ID.
telemetrymean_24hrs <- telemetry %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean_24hrs = rollapply(volt, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean_24hrs = rollapply(rotate, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean_24hrs = rollapply(pressure, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean_24hrs = rollapply(vibration, width = 24, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean_24hrs, rotatemean_24hrs, pressuremean_24hrs, vibrationmean_24hrs) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()

head(telemetrymean_24hrs)

telemetrysd_24hrs <- telemetry %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd_24hrs = rollapply(volt, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd_24hrs = rollapply(rotate, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd_24hrs = rollapply(pressure, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd_24hrs = rollapply(vibration, width = 24, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd_24hrs, rotatesd_24hrs, pressuresd_24hrs, vibrationsd_24hrs) %>%
  filter(!is.na(voltsd_24hrs)) %>%
  ungroup()

head(telemetrysd_24hrs)

# merge columns of feature sets created earlier
telemetryfeat <- data.frame(telemetrymean, telemetrysd[,-c(1:2)]) 
telemetryfeat_24hrs <- data.frame(telemetrymean_24hrs, telemetrysd_24hrs[,-c(1:2)])
telemetryfeat <- telemetryfeat %>%
  left_join(telemetryfeat_24hrs, by = c("datetime", "machineID")) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()

head(telemetryfeat)
summary(telemetryfeat)


# Lag Features from Errors ----

# Similar to telemetry, errors also come with time-stamps. However, unlike telemetry that had numerical values, 
# errors have categorical values denoting the type of error that occured at a time-stamp. In this case, aggregating 
# methods such as averaging does not apply. Counting the different categories is a more viable approach where lagging 
# counts of different types of errors that occured in the lag window are calculated. Below we create such lagging counts 
# from the errors received.

# create a column for each error type
errorcount <- errors %>% select(datetime, machineID, errorID) %>% 
  mutate(error1 = as.integer(errorID == "error1"), 
         error2 = as.integer(errorID == "error2"),
         error3 = as.integer(errorID == "error3"),
         error4 = as.integer(errorID == "error4"),
         error5 = as.integer(errorID == "error5"))

# sum the duplicate errors in an hour
errorcount <- errorcount %>% 
  group_by(machineID,datetime)%>%
  summarise(error1sum = sum(error1), 
            error2sum = sum(error2), 
            error3sum = sum(error3), 
            error4sum = sum(error4), 
            error5sum = sum(error5)) %>%
  ungroup()

head(errorcount)

# align errors with telemetry datetime field
errorfeat <- telemetry %>% 
  select(datetime, machineID) %>%
  left_join(errorcount, by = c("datetime", "machineID"))

# replace missing values
errorfeat[is.na(errorfeat)] <- 0

head(errorfeat)
summary(errorfeat)

# count the number of errors of different types in the last 24 hours,  for every 3 hours
errorfeat <- errorfeat %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(error1count = rollapply(error1sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error2count = rollapply(error2sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error3count = rollapply(error3sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error4count = rollapply(error4sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error5count = rollapply(error5sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, error1count, error2count, error3count, error4count, error5count) %>%
  filter(!is.na(error1count)) %>% 
  ungroup()

head(errorfeat)
summary(errorfeat)

# Days since last replacement from maintenance ----

# A crucial data set in this example is the maintenance records which contain the information of component 
# replacement records. Possible features from this data set can be, for example, the number of replacements 
# of each component in the last 3 months to incorporate the frequency of replacements. However, more relevent 
# information would be to calculate how long it has been since a component is last replaced as that would be 
# expected to correlate better with component failures since the longer a component is used, the more degradation 
# should be expected.

# As a side note, creating lagging features from maintenance data is not as sensible as it is for telemetry and errors 
# so the features from this data are generated in a more custom way. This type of ad-hoc feature engineering is very 
# common in predictive maintenance since domain knowledge plays a big role in understanding the predictors of a problem. 
# In the following, the days since last component replacement are calculated for each component type as features from 
# the maintenance data.

# create a binary column for each component. 1 if replacement occured, 0 if not.
comprep <- maint %>% 
  select(datetime, machineID, comp) %>% 
  mutate(comp1 = as.integer(comp == "comp1"), 
         comp2 = as.integer(comp == "comp2"),
         comp3 = as.integer(comp == "comp3"),
         comp4 = as.integer(comp == "comp4")) %>%
  select(-comp)

head(comprep)

comprep <- as.data.table(comprep)
setkey(comprep, machineID, datetime)

# seperate different component type replacements into different tables
comp1rep <- comprep[comp1 == 1, .(machineID, datetime, lastrepcomp1 = datetime)]# component 1 replacements
comp2rep <- comprep[comp2 == 1, .(machineID, datetime, lastrepcomp2 = datetime)]# component 2 replacements
comp3rep <- comprep[comp3 == 1, .(machineID, datetime, lastrepcomp3 = datetime)]# component 3 replacements
comp4rep <- comprep[comp4 == 1, .(machineID, datetime, lastrepcomp4 = datetime)]# component 4 replacements

# use telemetry feature table datetime and machineID to be matched with replacements
compdate <- as.data.table(telemetryfeat[,c(1:2)]) 
setkey(compdate, machineID, datetime)

# data.table rolling match will attach the latest record from the component replacement tables 
# to the telemetry date time and machineID
comp1feat <- comp1rep[compdate[,.(machineID, datetime)],roll = TRUE] 
comp1feat$sincelastcomp1 <- as.numeric(difftime(comp1feat$datetime, comp1feat$lastrepcomp1, units = "days"))
comp2feat <- comp2rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp2feat$sincelastcomp2 <- as.numeric(difftime(comp2feat$datetime, comp2feat$lastrepcomp2, units = "days"))
comp3feat <- comp3rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp3feat$sincelastcomp3 <- as.numeric(difftime(comp3feat$datetime, comp3feat$lastrepcomp3, units="days"))
comp4feat <- comp4rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp4feat$sincelastcomp4 <- as.numeric(difftime(comp4feat$datetime, comp4feat$lastrepcomp4, units = "days"))

# merge all tables
compfeat <-data.frame(compdate, comp1feat[,.(sincelastcomp1)], comp2feat[,.(sincelastcomp2)],
                      comp3feat[,.(sincelastcomp3)],comp4feat[,.(sincelastcomp4)])

head(compfeat,10)

# Machine Features ----

# The machine features are used directly as they are since they hold descriptive information about the type of the 
# machines and their ages which is the years in service. If the years in service information has been received 
# in the form of dates denoting the date of first service then a transformation would have been necessary to turn 
# those into a numeric values indicating the years in service.

# Lastly, we merge all the feature data sets we created earlier to get the final feature matrix.

# telemetry and error features have the same datetime 
finalfeat <- data.frame(telemetryfeat, errorfeat[,-c(1:2)])

# merge with component features and machine features lastly
finalfeat <- finalfeat %>% 
  left_join(compfeat, by = c("datetime","machineID")) %>% 
  left_join(machines, by = c("machineID"))

head(finalfeat, 10)
cat("The final set of features are:",paste0(names(finalfeat), ","))

# The final set of features are: datetime, machineID, voltmean, rotatemean, pressuremean, vibrationmean, voltsd, 
# rotatesd, pressuresd, vibrationsd, voltmean_24hrs, rotatemean_24hrs, pressuremean_24hrs, vibrationmean_24hrs, 
# voltsd_24hrs, rotatesd_24hrs, pressuresd_24hrs, vibrationsd_24hrs, error1count, error2count, error3count, 
# error4count, error5count, sincelastcomp1, sincelastcomp2, sincelastcomp3, sincelastcomp4, model, age

# Label Construction ----

# When using multi-class classification for predicting failure due to a problem, labeling is done by taking a time window 
# prior to the failure of an asset and labeling the feature records that fall into that window as “about to fail due 
# to a problem” while labeling all other records as “normal”. This time window should be picked according to the 
# business case where in some situations it may be enough to predict failures hours in advance while in others days 
# or weeks maybe needed to allow for the arrival of parts to be replaced as an example.

# The prediction problem for this example scenerio is to estimate the probability that a machine will fail in the near 
# future due to a failure of a certain component. More specifically, the goal is to compute the probability that a 
# machine will fail in the next 24 hours due to a certain component failure (component 1,2,3 or 4). In the following, 
# labelling is done by labeling all the feature records that fall into the 24 hours window before a failure due to 
# component 1, component 2, component 3 and component 4 as comp1, comp2, comp3 and comp4 respectively.The rest of 
# the records are labeled as "none" indicating, there is no failure within the next 24 hours.

# left join final features with failures on machineID then mutate a column for datetime difference
# filter date difference for the prediction horizon which is 24 hours
labeled <- left_join(finalfeat, failures, by = c("machineID")) %>%
  mutate(datediff = difftime(datetime.y, datetime.x, units = "hours")) %>%
  filter(datediff <= 24, datediff >= 0)

# left join labels to final features and fill NA's with "none" indicating no failure
labeledfeatures <- left_join(finalfeat, 
                             labeled %>% select(datetime.x, machineID, failure),
                             by = c("datetime" = "datetime.x", "machineID")) %>%
  arrange(machineID,datetime)

levels(labeledfeatures$failure) <- c(levels(labeledfeatures$failure), "none")
labeledfeatures$failure[is.na(labeledfeatures$failure)]<-"none"
head(labeledfeatures)

# Below is an example of records that are labeled as "comp4" in the failure column. First 8 records that fall into 
# the same 24 hours are all labeled as a block. Next 8 records that are within 24 hours of another component 4 
# failure are also labeled as "comp4" as a block.

head(labeledfeatures[labeledfeatures$failure == "comp4",], 16)

# Modeling / Model Development ----

# split at 2015-08-01 01:00:00, to train on the first 8 months and test on last 4 months
# labelling window is 24 hours so records within 24 hours prior to split point are left out
trainingdata1 <- labeledfeatures[labeledfeatures$datetime < "2015-07-31 01:00:00",] 
testingdata1 <- labeledfeatures[labeledfeatures$datetime > "2015-08-01 01:00:00",]

tail(trainingdata1)
head(testingdata1)

# split at 2015-09-01 01:00:00, to train on the first 9 months and test on last 3 months
# labelling window is 24 hours so records within 24 hours prior to split point are left out
trainingdata2 <- labeledfeatures[labeledfeatures$datetime < "2015-08-31 01:00:00",] 
testingdata2 <- labeledfeatures[labeledfeatures$datetime > "2015-09-01 01:00:00",]

tail(trainingdata2)
head(testingdata2)

# split at 2015-10-01 01:00:00, to train on the first 10 months and test on last 2 months
# labelling window is 24 hours so records within 24 hours prior to split point are left out
trainingdata3 <- labeledfeatures[labeledfeatures$datetime < "2015-09-30 01:00:00",] 
testingdata3 <- labeledfeatures[labeledfeatures$datetime > "2015-10-01 01:00:00",]

tail(trainingdata3)
head(testingdata3)

install.packages("gbm")
library(gbm)

# create the training formula 
trainformula <- as.formula(paste('failure',
                                 paste(names(labeledfeatures)[c(3:29)],collapse=' + '),
                                 sep=' ~ '))
trainformula

# train model on 3 splits
set.seed(1234)
gbm_model1 <- gbm(formula = trainformula, data = trainingdata1, 
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)
gbm_model2 <- gbm(formula = trainformula, data = trainingdata2, 
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)
gbm_model3 <- gbm(formula = trainformula, data = trainingdata3,
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)

# print relative influence of variables for 1st model as an example
summary(gbm_model1)


# Model Evaluation ----

# label distribution after features are labeled - the class imbalance problem
ggplot(labeledfeatures, aes(x=failure)) + 
  geom_bar(fill="red") + 
  labs(title = "label distribution", x = "labels")

# define evaluate function
Evaluate<-function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    actual = actual[!is.na(actual)]
    predicted = predicted[!is.na(predicted)]
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  
  #accuracy
  accuracy = sum(diag) / n
  
  #per class
  recall = diag / rowsums
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  
  #macro
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  
  #1-vs-all matrix
  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})
  
  s = matrix(0, nrow=2, ncol=2)
  for(i in 1:nc){s=s+oneVsAll[[i]]}
  
  #avg accuracy
  avgAccuracy = sum(diag(s))/sum(s)
  
  #micro
  microPrf = (diag(s) / apply(s,1, sum))[1];
  
  #majority class
  mcIndex = which(rowsums==max(rowsums))[1] # majority-class index
  mcAccuracy = as.numeric(p[mcIndex]) 
  mcRecall = 0*p;  mcRecall[mcIndex] = 1
  mcPrecision = 0*p; mcPrecision[mcIndex] = p[mcIndex]
  mcF1 = 0*p; mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] + 1)
  
  #random accuracy
  expAccuracy = sum(p*q)
  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  
  #random guess
  rgAccuracy = 1 / nc
  rgPrecision = p
  rgRecall = 0*p + 1 / nc
  rgF1 = 2 * p / (nc * p + 1)
  
  #rnd weighted
  rwgAccurcy = sum(p^2)
  rwgPrecision = p
  rwgRecall = p
  rwgF1 = p
  
  classNames = names(diag)
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  
  return(list(
    ConfusionMatrix = cm,
    Metrics = data.frame(
      Class = classNames,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      MacroAvgPrecision = macroPrecision,
      MacroAvgRecall = macroRecall,
      MacroAvgF1 = macroF1,
      AvgAccuracy = avgAccuracy,
      MicroAvgPrecision = microPrf,
      MicroAvgRecall = microPrf,
      MicroAvgF1 = microPrf,
      MajorityClassAccuracy = mcAccuracy,
      MajorityClassPrecision = mcPrecision,
      MajorityClassRecall = mcRecall,
      MajorityClassF1 = mcF1,
      Kappa = kappa,
      RandomGuessAccuracy = rgAccuracy,
      RandomGuessPrecision = rgPrecision,
      RandomGuessRecall = rgRecall,
      RandomGuessF1 = rgF1,
      RandomWeightedGuessAccurcy = rwgAccurcy,
      RandomWeightedGuessPrecision = rwgPrecision,
      RandomWeightedGuessRecall= rwgRecall,
      RandomWeightedGuessWeightedF1 = rwgF1)))
}

# evaluation metrics for first split
pred_gbm1 <- as.data.frame(predict(gbm_model1, testingdata1, 
                                   n.trees = 50,type = "response"))

names(pred_gbm1) <- gsub(".50", "", names(pred_gbm1))
pred_gbm1$failure <- as.factor(colnames(pred_gbm1)[max.col(pred_gbm1)])

eval1 <- Evaluate(actual=testingdata1$failure,predicted=pred_gbm1$failure)
eval1$ConfusionMatrix
t(eval1$Metrics)

# evaluation metrics for second split
pred_gbm2 <- as.data.frame(predict(gbm_model2, testingdata2,  
                                   n.trees = 50,type = "response"))

names(pred_gbm2) <- gsub(".50", "", names(pred_gbm2))
pred_gbm2$failure <- as.factor(colnames(pred_gbm2)[max.col(pred_gbm2)])

eval2 <- Evaluate(actual=testingdata2$failure,predicted=pred_gbm2$failure)
eval2$ConfusionMatrix
t(eval2$Metrics)

# evaluation metrics for third split
pred_gbm3 <- as.data.frame(predict(gbm_model3, testingdata3,  
                                   n.trees = 50,type = "response"))

names(pred_gbm3)<-gsub(".50", "", names(pred_gbm3))
pred_gbm3$failure <- as.factor(colnames(pred_gbm3)[max.col(pred_gbm3)])

eval3 <- Evaluate(actual=testingdata3$failure,predicted=pred_gbm3$failure)
eval3$ConfusionMatrix
t(eval3$Metrics)

# report the recall rates for the models
rownames <- c("comp1","comp2","comp3","comp4","none")
rownames
data.frame(cbind(failure = rownames,
                 gbm_model1_Recall = eval1$Metrics$Recall,
                 gbm_model2_Recall = eval2$Metrics$Recall,
                 gbm_model3_Recall = eval3$Metrics$Recall))





