library(caret)
library(MLmetrics)
source("utils.R")
datafolder <- '../data/Modeling_Data/'
datafile <- 'traindata_GLM.csv'
random_seed = 19924 # set random seed

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(paste(datafolder,datafile,sep=''),header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)
traindata <- trainval$traindata
validdata <- trainval$validdata


regformula <- PA ~. # Elephant presence is determined by a weighted sum of all features
logreg <- glm(formula=regformula,
              data=traindata[ , !(colnames(traindata) %in% c("HID"))],
              family=binomial)

summary(logreg) # view summary of fit

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


trainpred <- ifelse(trainpred_probs > thres, 1, 0)
AUC(trainpred, traindata$PA)


valpred <- ifelse(valpred_probs > thres, 1, 0)
AUC(valpred, validdata$PA)

# https://www.r-bloggers.com/2020/01/evaluate-your-r-model-with-mlmetrics/

          
