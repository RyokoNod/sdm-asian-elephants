# This script was made to check why there is a difference between
# my old Stan implementation of Bayesian linear GLM codes/old/bayesian_GLM.stan
# and the BRMS version of Bayesian linear GLM using brm().
# It uses the models created from the old implementation and 
# produces different versions of the test data prediction, future and present:
# 1. Final output draws are binary Bernoulli draws like the BRMS implementation
# 2. Final output draws are distributions of probabilities but draws are increased from 100->1000
# 3. Final output draws are Bernoulli and draws are increased

# Feb 11, 2022: 
# It makes no difference whether the original implementation's output distribution
# or draw size is changed. My original models' outputs still have a mismatch with
# the BRMS models.
library(caret)
library(rstan)
source("../../utils.R")

# settings
random_seed = 12244 # set random seed
datafolder <- '../../../data/Modeling_Data/'
resultfolder <- './'
feature_type <- 'GLM' # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- FALSE # TRUE if you want to normalize the data

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='')

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)

# separate the HIDs, labels, and the features
train_features <- subset(trainval$traindata, select=-c(HID, PA, Folds))
train_HID <- subset(trainval$traindata, select=c(HID))
train_labels <- subset(trainval$traindata, select=c(PA))

# if specified, normalize the data
if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  valid_features <- predict(preProc, valid_features)
}

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    model <- readRDS("../../old/bayesGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    model <- readRDS("../../old/bayesGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    model <- readRDS("../../old/bayesGLM_randCVfeat_model_TD11.rds") 
  }
  if (feature_type=="SGLM"){
    model <- readRDS("../../old/bayesGLM_spatialCVfeat_model.rds") 
  }
}


# Future predictions ------------------------------------------------------

testdata <- read.csv(testfile, header=TRUE) # import the future climate variables

# if specified, normalize data
if (normalize==TRUE){
  test_HID <- subset(testdata, select=HID)
  testdata <- predict(preProc, testdata[,2:dim(testdata)[2]])
  testdata <- cbind(test_HID,testdata)
}

# Changing predictions to Bernoulli draws
if (normalize==TRUE){
  test_matrixpath <- paste(resultfolder,'bayesGLM_pred_norm_',feature_type,'_',random_seed,'_bernoulli.rds',sep='')
  test_csvpath <- paste(resultfolder,'results_norm_',feature_type,'_', random_seed, '_bernoulli.csv',sep='')
}else{
  test_matrixpath <- paste(resultfolder,'bayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_bernoulli.rds',sep='')
  test_csvpath <- paste(resultfolder,'results_unnorm_',feature_type,'_', random_seed, '_bernoulli.csv',sep='')
}
bayesGLM_testpred(model=model, testdata=testdata, N=100, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath, 
                  replace=TRUE, bernoulli_draws= TRUE, seed=random_seed)

# Keeping the draws as probabilities, increasing sample size
if (normalize==TRUE){
  test_matrixpath <- paste(resultfolder,'bayesGLM_pred_norm_',feature_type,'_',random_seed,'_moredraws.rds',sep='')
  test_csvpath <- paste(resultfolder,'results_norm_',feature_type,'_', random_seed, '_moredraws.csv',sep='')
}else{
  test_matrixpath <- paste(resultfolder,'bayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_moredraws.rds',sep='')
  test_csvpath <- paste(resultfolder,'results_unnorm_',feature_type,'_', random_seed, '_moredraws.csv',sep='')
}
bayesGLM_testpred(model=model, testdata=testdata, N=1000, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath, 
                  replace=TRUE, bernoulli_draws= FALSE, seed=random_seed)


# Changing to Bernoulli draws AND increasing sample size
if (normalize==TRUE){
  test_matrixpath <- paste(resultfolder,'bayesGLM_pred_norm_',feature_type,'_',random_seed,'_morebernoullidraws.rds',sep='')
  test_csvpath <- paste(resultfolder,'results_norm_',feature_type,'_', random_seed, '_morebernoullidraws.csv',sep='')
}else{
  test_matrixpath <- paste(resultfolder,'bayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_morebernoullidraws.rds',sep='')
  test_csvpath <- paste(resultfolder,'results_unnorm_',feature_type,'_', random_seed, '_morebernoullidraws.csv',sep='')
}
bayesGLM_testpred(model=model, testdata=testdata, N=1000, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath, 
                  replace=TRUE, bernoulli_draws= TRUE, seed=random_seed)



# Present-day predictions -------------------------------------------------

pres_testdata <- read.csv(pres_testfile, header=TRUE) # import the present climate variables

# if specified, normalize data
if (normalize==TRUE){
  prestest_HID <- subset(pres_testdata, select=HID)
  pres_testdata <- predict(preProc, pres_testdata[,2:dim(pres_testdata)[2]])
  pres_testdata <- cbind(test_HID,pres_testdata)
}


# Changing predictions to Bernoulli draws
if (normalize==TRUE){
  pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_norm_',feature_type,'_',random_seed,'_bernoulli.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'results_pres_norm_',feature_type,'_', random_seed, '_bernoulli.csv',sep='')
}else{
  pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_bernoulli.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'results_pres_unnorm_',feature_type,'_', random_seed, '_bernoulli.csv',sep='')
}
bayesGLM_testpred(model=model, testdata=pres_testdata, N=100, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath, 
                  replace=TRUE, bernoulli_draws= TRUE, seed=random_seed)

# Keeping the draws as probabilities, increasing sample size
if (normalize==TRUE){
  pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_norm_',feature_type,'_',random_seed,'_moredraws.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'results_pres_norm_',feature_type,'_', random_seed, '_moredraws.csv',sep='')
}else{
  pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_moredraws.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'results_pres_unnorm_',feature_type,'_', random_seed, '_moredraws.csv',sep='')
}
bayesGLM_testpred(model=model, testdata=pres_testdata, N=1000, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath, 
                  replace=TRUE, bernoulli_draws= FALSE, seed=random_seed)

# Changing to Bernoulli draws AND increasing sample size
if (normalize==TRUE){
  pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_norm_',feature_type,'_',random_seed,'_morebernoullidraws.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'results_pres_norm_',feature_type,'_', random_seed, '_morebernoullidraws.csv',sep='')
}else{
  pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_morebernoullidraws.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'results_pres_unnorm_',feature_type,'_', random_seed, '_morebernoullidraws.csv',sep='')
}
bayesGLM_testpred(model=model, testdata=pres_testdata, N=1000, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath, 
                  replace=TRUE, bernoulli_draws= TRUE, seed=random_seed)





