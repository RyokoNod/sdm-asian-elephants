library("rstan")
library("loo")
source("utils.R")
library(tidyverse)

random_seed = 12244 # set random seed
datafolder <- '../data/Modeling_Data/'
resultfolder <- '../data/Results/Bayesian_SGLM/'

# GLM for random CV feature set, SGLM for spatial CV feature set
feature_type <- 'GLM'

trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')

# present day data on land. Overlaps with training and validation data
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='')

##### Preparing the training, validation, and test data #####

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# import the future climate variables
testdata <- read.csv(testfile, header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)
train_features <- subset(trainval$traindata, select=-c(PA))
train_labels <- subset(trainval$traindata, select=c(PA))
valid_features <- subset(trainval$validdata, select=-c(PA))
valid_labels <- subset(trainval$validdata, select=c(PA))

# prepare data for use in Stan
data <- list(
  K = dim(train_features)[2], # number of features
  Ntr = dim(train_features)[1], # number of training instances
  Nval = dim(valid_features)[1], # number of validation instances
  x = train_features, # training features
  y = train_labels[["PA"]], # training labels
  x_val = valid_features # validation features
)

##### Running Stan Model #####
#fit model and get draws
sm <- rstan::stan_model(file = "./bayesian_GLM.stan") # specifying where the Stan model is
model <- rstan::sampling(sm, data=data) # run MCMC

