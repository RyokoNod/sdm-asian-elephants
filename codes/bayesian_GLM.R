library(rstan)
library(loo)
source("utils.R")
library(shinystan)
options(mc.cores=parallel::detectCores())  # use all available cores

random_seed = 12244 # set random seed
datafolder <- '../data/Modeling_Data/'
resultfolder <- '../data/Results/Bayesian_GLM/'

# GLM for random CV feature set, SGLM for spatial CV feature set
feature_type <- 'SGLM'

trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='') # present day climatic

##### Preparing the training, validation, and test data #####

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)

# separate the HIDs, labels, and the features
train_features <- subset(trainval$traindata, select=-c(HID, PA))
train_HID <- subset(trainval$traindata, select=c(HID))
train_labels <- subset(trainval$traindata, select=c(PA))
valid_features <- subset(trainval$validdata, select=-c(HID, PA))
valid_labels <- subset(trainval$validdata, select=c(PA))
valid_HID <- subset(trainval$validdata, select=c(HID))

##### Running Stan model #####

# prepare data for use in Stan
data <- list(
  K = dim(train_features)[2], # number of features
  Ntr = dim(train_features)[1], # number of training instances
  Nval = dim(valid_features)[1], # number of validation instances
  x = train_features, # training features
  y = train_labels[["PA"]], # training labels
  x_val = valid_features # validation features
)

#fit model and get draws
sm <- rstan::stan_model(file = "./bayesian_GLM.stan") # specifying where the Stan model is
model <- rstan::sampling(sm, data=data, seed = random_seed,
                         control = list(adapt_delta = 0.99, max_treedepth = 10)) # run MCMC

# save model so I can recover if R crashes
if (feature_type=="GLM"){
  saveRDS(model, "bayesGLM_randCVfeat_model.rds") 
}
if (feature_type=="SGLM"){
  saveRDS(model, "bayesGLM_spatialCVfeat_model.rds")
}

# load if R crashes
if (feature_type=="GLM"){
  model <- readRDS("bayesGLM_randCVfeat_model.rds") 
}
if (feature_type=="SGLM"){
  model <- readRDS("bayesGLM_spatialCVfeat_model.rds") 
}

##### Predictions with test data (future) #####

testdata <- read.csv(testfile, header=TRUE) # import the future climate variables

# setup filepaths to save results
test_matrixpath <- paste(resultfolder,'bayesGLM_pred_',feature_type,'_',random_seed,'.rds',sep='')
test_csvpath <- paste(resultfolder,'results_',feature_type,'_', random_seed, '.csv',sep='')

# a function that does predictive posterior sampling and saves the results in specified file
bayesGLM_testpred(model=model, testdata=testdata, N=100, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath, seed=random_seed)

##### Predictions with test data (present) #####

pres_testdata <- read.csv(pres_testfile, header=TRUE) # import the present climate variables

pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_',feature_type,'_',random_seed,'.rds',sep='')
pres_test_csvpath <- paste(resultfolder,'results_pres_',feature_type,'_', random_seed, '.csv',sep='')

bayesGLM_testpred(model=model, testdata=pres_testdata, N=100, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath, seed=random_seed)

##### Training and Validation performance #####
evals <- tranval_metrics(model=model, traindata=trainval$traindata, 
                          valdata=trainval$validdata)
formattable(evals)


##### Model statistics #####
# see model statistics in shinystan
my_sso <- launch_shinystan(model)


