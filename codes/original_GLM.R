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


summary(logreg)
trainpred_probs <-predict(logreg,type="response")
trainpred <- ifelse(trainpred_probs > 0.5, 1, 0)
valpred_probs <- predict(logreg,newdata=validdata, type="response")
valpred <- ifelse(valpred_probs > 0.5, 1, 0)
