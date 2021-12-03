library(caret)
library(MLmetrics)
library(formattable)
source("utils.R")
random_seed = 19924 # set random seed
datafolder <- '../data/Modeling_Data/'
resultfolder <- '../data/Results/Original_GLM/'
mode <- 'GLM' # set 'GLM' or 'SGLM' to switch feature sets

trainfile <- paste(datafolder,'traindata_',mode,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',mode,'.csv',sep='')

##### Preparing the training, validation, and test data #####

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# import the future climate variables
testdata <- read.csv(testfile, header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)
traindata <- trainval$traindata
validdata <- trainval$validdata

##### Fitting the model #####

regformula <- PA ~. # Elephant presence is determined by a weighted sum of all features
logreg <- glm(formula=regformula,
              data=traindata[ , !(colnames(traindata) %in% c("HID"))],
              family=binomial)

summary(logreg) # view summary of fit

##### Evaluating the model #####

# the raw outputs (probabilities) of logistic regression model
trainpred_probs <-predict(logreg,type="response")
valpred_probs <- predict(logreg,newdata=validdata, type="response")

# get the ideal threshold value for elephant presence
# by maximizing the validation data's F1 score
thres_candidates <- seq(0.01, 0.99, .01)
f1_scores <- sapply(thres_candidates, 
                    function(thres) F1_Score(validdata$PA, 
                                             ifelse(valpred_probs >= thres, 1, 0), 
                                             positive = 1))
thres <- thres_candidates[which.max(f1_scores)] # threshold that maximizes F1 score

# get the precision, recall, accuracy, and AUC scores
trainpred <- ifelse(trainpred_probs > thres, 1, 0)
trainprec <- round(Precision(traindata$PA, trainpred, positive = 1), 3)
trainrec <- round(Recall(traindata$PA, trainpred, positive = 1), 3)
trainacc <- round(Accuracy(trainpred, traindata$PA), 3)
trainauc <- round(AUC(trainpred, traindata$PA), 3)

valpred <- ifelse(valpred_probs > thres, 1, 0)
valprec <- round(Precision(validdata$PA, valpred, positive = 1), 3)
valrec <- round(Recall(validdata$PA, valpred, positive = 1), 3)
valacc <- round(Accuracy(valpred, validdata$PA), 3)
valauc <- round(AUC(valpred, validdata$PA), 3)

# display the evaluation metrics as tables
evals <- data.frame(Dataset = c("Training", "Validation"),
                    Precision = c(trainprec, valprec),
                    Recall = c(trainrec,  valrec),
                    Accuracy = c(trainacc, valacc),
                    AUC = c(trainauc, valauc)
                    )
formattable(evals)

##### Test data results ######

# the outputs (probabilities) of logistic regression model
testpred_probs <-predict(logreg,newdata=testdata,type="response")
testpred <- ifelse(testpred_probs > thres, 1, 0)
testpred <- cbind(testdata['HID'], testpred_probs, testpred)
names(testpred) <- c('HID', 'probs', 'pred_labels')


# write to file for QGIS visualization
resultfile <- paste(resultfolder,'results_',mode,'_thres', thres*100,'.csv',sep='')
write.csv(testpred, resultfile, row.names=FALSE)
