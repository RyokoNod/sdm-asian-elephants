library(caret)
library(randomForest)
library(MLmetrics)
library(dplyr)
library(formattable)
library(reliabilitydiag)
source("../utils.R")
random_seed = 12244 # set random seed
datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Standard_RF/'
mode <- 'SGLM' # set 'GLM' or 'SGLM' to switch feature sets
niter <- 100 # each prediction is the mean of niter random forests

trainfile <- paste(datafolder,'traindata_',mode,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',mode,'.csv',sep='')

# present day data on land. Overlaps with training and validation data
pres_testfile <- paste(datafolder,'testdata_pres_',mode,'.csv',sep='')

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)


# Spatial cross validation ------------------------------------------------
nfolds <- length(unique(traindata_master$Folds))
eval_metrics <- c("AUC", "TSS", "sensitivity", "specificity") # evaluation metric list

# create matrices to store evaluations for each fold
train_evals <- matrix(nrow=nfolds, ncol=length(eval_metrics))
val_evals <- matrix(nrow=nfolds, ncol=length(eval_metrics))
colnames(train_evals)<- eval_metrics
colnames(val_evals)<- eval_metrics

valpreds_all <- data.frame() # an empty data frame to store validation predictions

for (f in 1:nfolds){
  # create training data and validation data for each fold
  # also create matrix to store predictions for the niter forests
  traindata <- subset(traindata_master, Folds!=f)
  valdata <- subset(traindata_master, Folds==f)
  trainpreds <- matrix(nrow = nrow(traindata), ncol = niter)
  valpreds <- matrix(nrow = nrow(valdata), ncol = niter)
  
  # create predictions for niter random forests
  for (i in 1:niter){
    set.seed(random_seed + i - 1)
    rf <- randomForest(as.factor(PA) ~ ., 
                       data = subset(traindata, select=-c(HID, Folds)),
                       ntree = 100)
    trainpreds[,i] <- predict(rf, type = "prob")[, "1"]
    valpreds[,i] <- predict(rf, newdata = subset(valdata, select=-c(HID, Folds)),
                                                 type = "prob")[, "1"]
  }
  
  # average the niter predictions to create one prediction for each hexagon
  trainpred <- apply(trainpreds, 1, mean)
  valpred <- apply(valpreds, 1, mean)
  valpreds_all <- rbind(valpreds_all, cbind(valdata["HID"], valpred))
  
  # calculate AUC for the fold and store
  train_evals[f, "AUC"] <- AUC(trainpred, traindata$PA)
  val_evals[f, "AUC"] <- AUC(valpred, valdata$PA)
  
  # calculate TSS, sensitivity, specificity for the fold and store
  train_TSSscores <- maxTSS_scores(trainpred, traindata$PA)
  val_TSSscores <- maxTSS_scores(valpred, valdata$PA)
  train_evals[f, "TSS"] <- train_TSSscores$TSS
  val_evals[f, "TSS"] <- val_TSSscores$TSS
  train_evals[f, "sensitivity"] <- train_TSSscores$sensitivity
  val_evals[f, "sensitivity"] <- val_TSSscores$sensitivity
  train_evals[f, "specificity"] <- train_TSSscores$specificity
  val_evals[f, "specificity"] <- val_TSSscores$specificity
}

# Save the validation predictions because RStudio tends to crash after this point
# I will use these later for calibration plots
valpreds_all <- merge(subset(traindata_master, select=c(HID, PA)), valpreds_all, by="HID")
valpreds_file <-  paste(resultfolder,'valpreds_RF_',mode,'_seed', random_seed, 
                        '.csv',sep='')
write.csv(valpreds_all, valpreds_file, row.names=FALSE)

# the final evaluation scores are the mean of the CV folds
evals <- t(data.frame(apply(train_evals, 2, mean), apply(val_evals, 2, mean)))
evals <- data.frame(evals) %>% mutate_if(is.numeric, round, digits=3)
row.names(evals) <- c("Training", "Validation")

formattable(evals) # display scores



# Present-day predictions -------------------------------------------------

pres_testdata <- read.csv(pres_testfile, header=TRUE)
preds <- matrix(nrow = nrow(pres_testdata), ncol = niter)

# create predictions for niter random forests
for (i in 1:niter){
  set.seed(random_seed + i - 1)
  rf <- randomForest(as.factor(PA) ~ ., 
                     data = subset(traindata_master, select = -c(HID,Folds))
                     , ntree = 100)
  preds[,i] <- predict(rf, newdata = subset(pres_testdata, select=-HID), 
                       type = "prob")[, "1"]
}

# average the predictions, combine with HID
preds <- apply(preds, 1, mean)
pres_testpred <- cbind(pres_testdata['HID'], preds)

# write to file
pres_resultfile <- paste(resultfolder,'results_present_RF_',mode,'_seed', random_seed, 
                         '.csv',sep='')
write.csv(pres_testpred, pres_resultfile, row.names=FALSE)



# Future predictions ------------------------------------------------------

testdata <- read.csv(testfile, header=TRUE)
preds <- matrix(nrow = nrow(testdata), ncol = niter)

# create predictions for niter random forests
for (i in 1:niter){
  set.seed(random_seed + i - 1)
  rf <- randomForest(as.factor(PA) ~ ., 
                     data = subset(traindata_master, select = -c(HID,Folds))
                     , ntree = 100)
  preds[,i] <- predict(rf, newdata = subset(testdata, select=-HID), 
                       type = "prob")[, "1"]
}

# average the predictions, combine with HID
preds <- apply(preds, 1, mean)
testpred <- cbind(testdata['HID'], preds)

# write to file
resultfile <- paste(resultfolder,'results_RF_',mode,'_seed', random_seed, 
                         '.csv',sep='')
write.csv(testpred, resultfile, row.names=FALSE)



# Calibration plots -------------------------------------------------------

# load in case RStudio crashed
valpreds_file <-  paste(resultfolder,'valpreds_RF_',mode,'_seed', random_seed, 
                        '.csv',sep='')
valpreds_all <- read.csv(valpreds_file)

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(valpreds_all$PA) ~ random_forest, 
                         data = data.frame(random_forest=valpreds_all$valpred,
                                           y=factor(valpreds_all$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData)

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = valpreds_all$valpred, y = valpreds_all$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted Probabilities",
       y="Conditional event probabilities")+
  bayesplot::theme_default(base_family = "sans")




