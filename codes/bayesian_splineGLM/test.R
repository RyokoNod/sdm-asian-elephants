library(caret)
library(brms)
library(MLmetrics)
library(dplyr)
library(formattable)
library(reliabilitydiag)
library(shinystan)
source("../utils.R")
options(mc.cores=parallel::detectCores())  # use all available cores

# settings
random_seed = 12244 # set random seed
datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_splineGLM/'
CVmodelfolder <- './CV_intermediate_models/'
feature_type <- 'GLM' # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- FALSE # TRUE if you want to normalize the data
adapt_d <- 0.99
treedepth <- 12

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='') # present day climatic

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)


# Define formula and priors -----------------------------------------------

if (feature_type=="GLM"){
  formula <- as.factor(PA) ~ 0 + Intercept + s(BIO03_Mean) + s(TN10P_IDW1N10) +
    s(GSL_IDW1N10) + s(TNX_IDW1N10) + s(ID_IDW1N10) + s(BIO14_Mean) + s(BIO18_Mean) +
    s(CWD_IDW1N10) + s(RX1DAY_IDW1N10) + s(WSDI_IDW1N10)
  priors <- c(set_prior("normal(0,5)", class="b", coef="sBIO03_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sTN10P_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sGSL_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sTNX_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sID_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sBIO14_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sBIO18_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sCWD_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sRX1DAY_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sWSDI_IDW1N10_1"),
              set_prior("normal(0,10)", class="b", coef="Intercept"))
}else{
  formula <- as.factor(PA) ~ 0 + Intercept + s(BIO08_Mean) + s(TXX_IDW1N10) +
    s(BIO02_Mean) + s(TN90P_IDW1N10) + s(ID_IDW1N10) + s(BIO14_Mean) + 
    s(BIO18_Mean) + s(CWD_IDW1N10) + s(RX1DAY_IDW1N10) + s(WSDI_IDW1N10)
  priors <- c(set_prior("normal(0,5)", class="b", coef="sBIO08_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sTXX_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sBIO02_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sTN90P_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sID_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sBIO14_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sBIO18_Mean_1"),
              set_prior("normal(0,5)", class="b", coef="sCWD_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sRX1DAY_IDW1N10_1"),
              set_prior("normal(0,5)", class="b", coef="sWSDI_IDW1N10_1"),
              set_prior("normal(0,10)", class="b", coef="Intercept"))
}


# Present-day predictions -------------------------------------------------

# separate the labels and features from the master training data
train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
train_labels <- subset(traindata_master, select=c(PA))

# read present-day test data, separate features
pres_testdata <- read.csv(pres_testfile, header=TRUE)
pres_testfeatures <- subset(pres_testdata, select=-c(HID))

if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  pres_testfeatures <- predict(preProc, pres_testfeatures)
  pres_testdata <- cbind(HID=pres_testdata$HID, pres_testfeatures) # put HID together again
}

# create model
bsplineGLM <- brm(formula=formula, 
                  data=cbind(train_features, train_labels),
                  family=bernoulli(link="logit"),
                  prior=priors,
                  control = list(adapt_delta = adapt_d, max_treedepth = treedepth),
                  seed=random_seed
)

# save model so I can recover if R crashes
if (normalize==TRUE){
  if (feature_type=="GLM"){
    saveRDS(bsplineGLM, "test_bayessplineGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    saveRDS(bsplineGLM, "test_bayessplineGLM_norm_spatialCVfeat_model.rds")
  }
} else{
  if (feature_type=="GLM"){
    saveRDS(bsplineGLM, "test_bayessplineGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    saveRDS(bsplineGLM, "test_bayessplineGLM_spatialCVfeat_model.rds")
  }
}