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
feature_type <- 'SGLM' # set 'GLM' or 'SGLM' to switch feature sets
normalize <-TRUE # TRUE if you want to normalize the data
niter <- 1 # each prediction is the mean of niter random forests

trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='')

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
  fold_train <- subset(traindata_master, Folds!=f)
  fold_val <- subset(traindata_master, Folds==f)
  trainpreds <- matrix(nrow = nrow(fold_train), ncol = niter)
  valpreds <- matrix(nrow = nrow(fold_val), ncol = niter)
  
  # separate the HIDs, labels, and the features
  traindata <- list("features"=subset(fold_train, select=-c(HID, PA, Folds)),
                    "labels"=subset(fold_train, select=c(PA)),
                    "HID"= subset(fold_train, select=c(HID)))
  valdata <- list("features"=subset(fold_val, select=-c(HID, PA, Folds)),
                  "labels"=subset(fold_val, select=c(PA)),
                  "HID"= subset(fold_val, select=c(HID)))
  
  # if specified, normalize the data
  if (normalize==TRUE){
    preProc <- preProcess(traindata$features, method=c("range"))
    traindata$features <- predict(preProc, traindata$features)
    valdata$features <- predict(preProc,  valdata$features)
  }
  
  # create predictions for niter random forests
  for (i in 1:niter){
    set.seed(random_seed + i - 1)
    rf <- randomForest(as.factor(PA) ~ ., 
                       data = cbind(traindata$features, traindata$labels),
                       ntree = 100)
    trainpreds[,i] <- predict(rf, type = "prob")[, "1"]
    valpreds[,i] <- predict(rf, newdata = cbind(valdata$features, valdata$labels),
                            type = "prob")[, "1"]
  }
  
  # average the niter predictions to create one prediction for each hexagon
  trainpred <- apply(trainpreds, 1, mean)
  valpred <- apply(valpreds, 1, mean)
  valpreds_all <- rbind(valpreds_all, cbind(HID=valdata$HID$HID, valpred))
  
  # calculate AUC for the fold and store
  train_evals[f, "AUC"] <- AUC(trainpred, traindata$labels$PA)
  val_evals[f, "AUC"] <- AUC(valpred, valdata$labels$PA)
  
  # calculate TSS, sensitivity, specificity for the fold and store
  train_TSSscores <- maxTSS_scores(trainpred, traindata$labels$PA)
  val_TSSscores <- maxTSS_scores(valpred, valdata$labels$PA)
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

if (normalize==TRUE){
  valpreds_file <-  paste(resultfolder,'valpreds_RF_norm',feature_type,'_seed', random_seed, 
                          '.csv',sep='')
}else{
  valpreds_file <-  paste(resultfolder,'valpreds_RF_',feature_type,'_seed', random_seed, 
                          '.csv',sep='')
}
write.csv(valpreds_all, valpreds_file, row.names=FALSE)

# the final evaluation scores are the mean of the CV folds
evals <- t(data.frame(apply(train_evals, 2, mean), apply(val_evals, 2, mean)))
evals <- data.frame(evals) %>% mutate_if(is.numeric, round, digits=3)
row.names(evals) <- c("Training", "Validation")

formattable(evals) # display scores



# Present-day predictions -------------------------------------------------

pres_testdata <- read.csv(pres_testfile, header=TRUE)
pres_testfeatures <- subset(pres_testdata, select=-c(HID))

# separate the labels and features from the master training data
train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
train_labels <- subset(traindata_master, select=c(PA))

if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  pres_testfeatures <- predict(preProc, pres_testfeatures)
  pres_testdata <- cbind(HID=pres_testdata$HID, pres_testfeatures) # put HID together again
}

preds <- matrix(nrow = nrow(pres_testdata), ncol = niter)

# create predictions for niter random forests
for (i in 1:niter){
  set.seed(random_seed + i - 1)
  rf <- randomForest(as.factor(PA) ~ ., 
                     data = cbind(train_features, train_labels)
                     , ntree = 100)
  preds[,i] <- predict(rf, newdata = subset(pres_testdata, select=-HID), 
                       type = "prob")[, "1"]
}

# average the predictions, combine with HID
preds <- apply(preds, 1, mean)
pres_testpred <- cbind(pres_testdata['HID'], preds)

# write to file
if (normalize==TRUE){
  pres_resultfile <- paste(resultfolder,'results_present_RF_norm',feature_type,'_seed', random_seed, 
                           '.csv',sep='')
}else{
  pres_resultfile <- paste(resultfolder,'results_present_RF_',feature_type,'_seed', random_seed, 
                           '.csv',sep='')
}

write.csv(pres_testpred, pres_resultfile, row.names=FALSE)



# Future predictions ------------------------------------------------------

# separate the HIDs, labels, and the features from the master training data
train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
train_labels <- subset(traindata_master, select=c(PA))

testdata <- read.csv(testfile, header=TRUE)
testfeatures <- subset(testdata, select=-c(HID))

if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  testfeatures <- predict(preProc, testfeatures)
  testdata <- cbind(HID=testdata$HID, testfeatures) # put HID together again
}

preds <- matrix(nrow = nrow(testdata), ncol = niter)

# create predictions for niter random forests
for (i in 1:niter){
  set.seed(random_seed + i - 1)
  rf <- randomForest(as.factor(PA) ~ ., 
                     data = cbind(train_features, train_labels)
                     , ntree = 100)
  preds[,i] <- predict(rf, newdata = subset(testdata, select=-HID), 
                       type = "prob")[, "1"]
}

# average the predictions, combine with HID
preds <- apply(preds, 1, mean)
testpred <- cbind(testdata['HID'], preds)

# write to file
if (normalize==TRUE){
  resultfile <- paste(resultfolder,'results_RF_norm',feature_type,'_seed', random_seed, 
                      '.csv',sep='')
}else{
  resultfile <- paste(resultfolder,'results_RF_',feature_type,'_seed', random_seed, 
                      '.csv',sep='')
}
write.csv(testpred, resultfile, row.names=FALSE)



# Calibration plots (validation folds) -------------------------------------------------------

# load in case RStudio crashed
if (normalize==TRUE){
  valpreds_file <-  paste(resultfolder,'valpreds_RF_norm',feature_type,'_seed', random_seed, 
                          '.csv',sep='')
}else{
  valpreds_file <-  paste(resultfolder,'valpreds_RF_',feature_type,'_seed', random_seed, 
                          '.csv',sep='')
}
valpreds_all <- read.csv(valpreds_file)

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(valpreds_all$PA) ~ random_forest, 
                         data = data.frame(random_forest=valpreds_all$valpred,
                                           y=factor(valpreds_all$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData)+ 
  labs(x="Predicted probabilities (percentage)", y="Observed event frequencies (percentage)")

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = valpreds_all$valpred, y = valpreds_all$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted probabilities",
       y="Observed event frequencies")+
  bayesplot::theme_default(base_family = "sans")

# Calibration plots (training data, final model) -------------------------------------------------------

# load training predictions
if (normalize==TRUE){
  pres_preds_file <-  paste(resultfolder,'results_present_RF_norm',feature_type,'_seed', random_seed, 
                            '.csv',sep='')
}else{
  pres_preds_file <-  paste(resultfolder,'results_present_RF_',feature_type,'_seed', random_seed, 
                            '.csv',sep='')
}
pres_preds <- read.csv(pres_preds_file)


# stick the presence/absence together
trainpreds <- filter(pres_preds, HID %in% traindata_master$HID)
trainpreds <- left_join(trainpreds, traindata_master[c("HID", "PA")], by="HID")

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(trainpreds$PA) ~ bayes_GLM, 
                         data = data.frame(bayes_GLM=trainpreds$preds,
                                           y=factor(trainpreds$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData) +
  labs(x="Predicted probabilities (percentage)", y="Observed event frequencies (percentage)")

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = trainpreds$preds, y = trainpreds$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted probabilities",
       y="Observed event frequencies")+
  bayesplot::theme_default(base_family = "sans")




