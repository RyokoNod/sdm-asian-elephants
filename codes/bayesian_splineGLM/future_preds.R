library(caret)
library(brms)
library(MLmetrics)
library(dplyr)
source("../utils.R")
options(mc.cores=parallel::detectCores())  # use all available cores

# settings
datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_splineGLM/'

args = commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==2) {
  args[3] = 12244
}

feature_type <- args[1] # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- as.logical(args[2]) # TRUE if you want to specify model for normalized features
random_seed = as.integer(args[3]) # random seed used in model name

print(paste("feature type: ", feature_type))
print(paste("normalize: ", normalize))
print(paste("random seed: ", random_seed))


# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# Future predictions ------------------------------------------------------

# separate the HIDs, labels, and the features from the master training data
train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
train_labels <- subset(traindata_master, select=c(PA))

# read future test data, separate features and HID
testdata <- read.csv(testfile, header=TRUE)
testfeatures <- subset(testdata, select=-c(HID))

if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  testfeatures <- predict(preProc, testfeatures)
  testdata <- cbind(HID=testdata$HID, testfeatures) # put HID together again
}


# load the models
if (normalize==TRUE){
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS("bayessplineGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS("bayessplineGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS("bayessplineGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS("bayessplineGLM_spatialCVfeat_model.rds") 
  }
}

# predict and save to files
if (normalize==TRUE){
  preds_matrix <-  paste(resultfolder,'preds_bayessplineGLM_',feature_type,'norm_seed', random_seed, 
                         '.rds',sep='')
  preds_file <-  paste(resultfolder,'preds_bayessplineGLM_',feature_type,'norm_seed', random_seed, 
                       '.csv',sep='')
}else{
  preds_matrix <-  paste(resultfolder,'preds_bayessplineGLM_',feature_type,'unnorm_seed', random_seed, 
                         '.rds',sep='')
  preds_file <-  paste(resultfolder,'preds_bayessplineGLM_',feature_type,'unnorm_seed', random_seed, 
                       '.csv',sep='')
}
print("Predicting and saving to file...")
brmsGLM_testpred(bsplineGLM, testdata, matrixpath=preds_matrix, csvpath=preds_file)
