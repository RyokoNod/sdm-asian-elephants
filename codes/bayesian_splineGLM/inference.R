library(caret)
library(brms)
library(MLmetrics)
library(dplyr)
library(shinystan)
source("../utils.R")
options(mc.cores=parallel::detectCores())  # use all available cores

datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_splineGLM/'

args = commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==4) {
  args[5] = 12244
}

feature_type <- args[1] # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- as.logical(args[2]) # TRUE if you want to normalize the data
adapt_d <- as.double(args[3]) # adapt_delta parameter for Stan. If no idea put 0.99
treedepth <- as.integer(args[4]) # tree depth parameter for Stan. Default is 10 but adjust accordingly
random_seed = as.integer(args[5]) # random seed parameter for Stan

print(paste("feature type: ", feature_type))
print(paste("normalize: ", normalize))
print(paste("adapt delta: ", adapt_d))
print(paste("maximum tree depth: ", treedepth))
print(paste("random seed: ", random_seed))

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')

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


# Inference -------------------------------------------------

# separate the labels and features from the master training data
train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
train_labels <- subset(traindata_master, select=c(PA))

if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
}

# create model
bsplineGLM <- brm(formula=formula, 
                  data=cbind(train_features, train_labels),
                  family=bernoulli(link="logit"),
                  prior=priors,
                  control = list(adapt_delta = adapt_d, max_treedepth = treedepth),
                  seed=random_seed
)

# save model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    saveRDS(bsplineGLM, "bayessplineGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    saveRDS(bsplineGLM, "bayessplineGLM_norm_spatialCVfeat_model.rds")
  }
} else{
  if (feature_type=="GLM"){
    saveRDS(bsplineGLM, "bayessplineGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    saveRDS(bsplineGLM, "bayessplineGLM_spatialCVfeat_model.rds")
  }
}