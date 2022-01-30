library(caret)
library(MLmetrics)
library(dplyr)
library(formattable)
library(reliabilitydiag)
source("../utils.R")
random_seed = 12244 # set random seed
datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Standard_linearGLM/'
vartype <- 'SGLM' # set 'GLM' or 'SGLM' to switch feature sets

trainfile <- paste(datafolder,'traindata_',vartype,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',vartype,'.csv',sep='')

# present day data, all land areas. Overlaps with training data
pres_testfile <- paste(datafolder,'testdata_pres_',vartype,'.csv',sep='')

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
  traindata <- subset(traindata_master, Folds!=f)
  valdata <- subset(traindata_master, Folds==f)
  
  # create the predictions
  set.seed(random_seed)
  logreg <- glm(formula=as.factor(PA) ~ ., 
                data = subset(traindata, select=-c(HID, Folds)),
                family=binomial)
  
  trainpred <-predict(logreg,type="response")
  valpred <- predict(logreg,newdata=subset(valdata, select=-c(HID, Folds))
                     , type="response")
  
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
valpreds_file <-  paste(resultfolder,'valpreds_slinGLM_',vartype,'_seed', random_seed, 
                        '.csv',sep='')
write.csv(valpreds_all, valpreds_file, row.names=FALSE)

# the final evaluation scores are the mean of the CV folds
evals <- t(data.frame(apply(train_evals, 2, mean), apply(val_evals, 2, mean)))
evals <- data.frame(evals) %>% mutate_if(is.numeric, round, digits=3)
row.names(evals) <- c("Training", "Validation")

formattable(evals) # display scores



# Present-day predictions -------------------------------------------------

pres_testdata <- read.csv(pres_testfile, header=TRUE)

# create predictions
logreg <- glm(formula=as.factor(PA) ~ ., 
              data = subset(traindata_master, select=-c(HID, Folds)),
              family=binomial)

preds <- predict(logreg,newdata=subset(pres_testdata, select=-HID)
                   , type="response")

# combine predictions with HID
pres_testpred <- cbind(pres_testdata['HID'], preds)

# write to file
pres_resultfile <- paste(resultfolder,'results_present_slinGLM_',vartype,'_seed', random_seed, 
                         '.csv',sep='')
write.csv(pres_testpred, pres_resultfile, row.names=FALSE)




# Future predictions ------------------------------------------------------

testdata <- read.csv(testfile, header=TRUE)


# create predictions
logreg <- glm(formula=as.factor(PA) ~ ., 
              data = subset(traindata_master, select=-c(HID, Folds)),
              family=binomial)

preds <- predict(logreg,newdata=subset(testdata, select=-HID), type="response")

# combine predictions with HID
testpred <- cbind(testdata['HID'], preds)

# write to file
resultfile <- paste(resultfolder,'results_slinGLM_',vartype,'_seed', random_seed, 
                         '.csv',sep='')
write.csv(testpred, resultfile, row.names=FALSE)




# Calibration plots -------------------------------------------------------

# load in case RStudio crashed
valpreds_file <-  paste(resultfolder,'valpreds_slinGLM_',vartype,'_seed', random_seed, 
                        '.csv',sep='')
valpreds_all <- read.csv(valpreds_file)

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(valpreds_all$PA) ~ standard_linGLM, 
                         data = data.frame(standard_linGLM=valpreds_all$valpred,
                                           y=factor(valpreds_all$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData)

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = valpreds_all$valpred, y = valpreds_all$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted Probabilities",
       y="Conditional event probabilities")+
  bayesplot::theme_default(base_family = "sans")









