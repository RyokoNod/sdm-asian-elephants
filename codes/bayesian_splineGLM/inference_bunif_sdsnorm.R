library(caret)
library(brms)
library(MLmetrics)
library(dplyr)
source("../utils.R")
options(mc.cores=parallel::detectCores())  # use all available cores

datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_splineGLM/'

args = commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==5) {
  args[6] = 12244
}

feature_type <- args[1] # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- as.logical(args[2]) # TRUE if you want to normalize the data
adapt_d <- as.double(args[3]) # adapt_delta parameter for Stan. If no idea put 0.99
treedepth <- as.integer(args[4]) # tree depth parameter for Stan. Default is 10 but adjust accordingly
k <- as.integer(args[5]) # each smooth will have k building block functions to approximate true function
random_seed = as.integer(args[6]) # random seed parameter for Stan


print(paste("feature type: ", feature_type))
print(paste("normalize: ", normalize))
print(paste("adapt delta: ", adapt_d))
print(paste("maximum tree depth: ", treedepth))
print(paste("basis functions k:", k))
print(paste("random seed: ", random_seed))

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)


# Define formula and priors -----------------------------------------------

if (feature_type=="GLM"){
  formula <- as.factor(PA) ~ 0 + Intercept + s(BIO03_Mean, k=k) + s(TN10P_IDW1N10, k=k) +
    s(GSL_IDW1N10, k=k) + s(TNX_IDW1N10, k=k) + s(ID_IDW1N10, k=k) + s(BIO14_Mean, k=k) + 
    s(BIO18_Mean, k=k) + s(CWD_IDW1N10, k=k) + s(RX1DAY_IDW1N10, k=k) + s(WSDI_IDW1N10, k=k)
  priors <- c(set_prior("normal(0,1)", class="sds", coef="s(BIO03_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(TN10P_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(GSL_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(TNX_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(ID_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(BIO14_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(BIO18_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(CWD_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(RX1DAY_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(WSDI_IDW1N10, k = k)"))
}else{
  formula <- as.factor(PA) ~ 0 + Intercept + s(BIO08_Mean, k=k) + s(TXX_IDW1N10, k=k) +
    s(BIO02_Mean, k=k) + s(TN90P_IDW1N10, k=k) + s(ID_IDW1N10, k=k) + s(BIO14_Mean, k=k) + 
    s(BIO18_Mean, k=k) + s(CWD_IDW1N10, k=k) + s(RX1DAY_IDW1N10, k=k) + s(WSDI_IDW1N10, k=k)
  priors <- c(set_prior("normal(0,1)", class="sds", coef="s(BIO08_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(TXX_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(BIO02_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(TN90P_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(ID_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(BIO14_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(BIO18_Mean, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(CWD_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(RX1DAY_IDW1N10, k = k)"),
              set_prior("normal(0,1)", class="sds", coef="s(WSDI_IDW1N10, k = k)"))
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
                  seed=random_seed,
                  iter=3000
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