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
saveRDS(model, "bayesGLM_spatialCVfeat_model.rds") # save model so I can recover if R crashes

# load if R crashes
model <- readRDS("bayesGLM_randCVfeat_model.rds") 

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

# setup filepaths to save results
pres_test_matrixpath <- paste(resultfolder,'presbayesGLM_pred_',feature_type,'_',random_seed,'.rds',sep='')
pres_test_csvpath <- paste(resultfolder,'results_pres_',feature_type,'_', random_seed, '.csv',sep='')

bayesGLM_testpred(model=model, testdata=pres_testdata, N=100, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath, seed=random_seed)

##### Training and Validation performance #####
library(MLmetrics) 
library(formattable)

draws <- extract(model) # get the sample draws from model
tr_probs <- draws$tr_probs
val_probs <- draws$val_probs
colnames(tr_probs) <- train_HID[["HID"]]
colnames(val_probs) <- valid_HID[["HID"]]

tr_probs_point <- apply(tr_probs, 2, mean)
val_probs_point <- apply(val_probs, 2, mean)

thres_candidates <- seq(0.01, 0.99, .01)
f1_scores <- sapply(thres_candidates, 
                    function(thres) F1_Score(valid_labels[["PA"]], 
                                             ifelse(val_probs_med >= thres, 1, 0), 
                                             positive = 1))
thres <- thres_candidates[which.max(f1_scores)]

# get the precision, recall, accuracy, and AUC scores
trainpred <- ifelse(tr_probs_point > thres, 1, 0)
trainprec <- round(Precision(train_labels[["PA"]], trainpred, positive = 1), 3)
trainrec <- round(Recall(train_labels[["PA"]], trainpred, positive = 1), 3)
trainacc <- round(Accuracy(trainpred, train_labels[["PA"]]), 3)
trainauc <- round(AUC(trainpred, train_labels[["PA"]]), 3)

valpred <- ifelse(val_probs_point > thres, 1, 0)
valprec <- round(Precision(valid_labels[["PA"]], valpred, positive = 1), 3)
valrec <- round(Recall(valid_labels[["PA"]], valpred, positive = 1), 3)
valacc <- round(Accuracy(valpred, valid_labels[["PA"]]), 3)
valauc <- round(AUC(valpred, valid_labels[["PA"]]), 3)

# display the evaluation metrics as tables
evals <- data.frame(Dataset = c("Training", "Validation"),
                    Precision = c(trainprec, valprec),
                    Recall = c(trainrec,  valrec),
                    Accuracy = c(trainacc, valacc),
                    AUC = c(trainauc, valauc)
)
formattable(evals)

##### Troubleshooting and Tuning #####

# names of columns in draws that contain coefficients and intercept
coeff_names <- c("coeffs[1]","coeffs[2]","coeffs[3]","coeffs[4]","coeffs[5]", 
                 "coeffs[6]","coeffs[7]","coeffs[8]","coeffs[9]","coeffs[10]",
                 "intercept")

# see model statistics in shinystan
my_sso <- launch_shinystan(model)


