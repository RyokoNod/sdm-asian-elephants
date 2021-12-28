library(rstan)
library(loo)
source("utils.R")
library(shinystan)
options(mc.cores=parallel::detectCores())  # use all available cores

random_seed = 12244 # set random seed
datafolder <- '../data/Modeling_Data/'
resultfolder <- '../data/Results/'

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
train_features <- subset(trainval$traindata, select=-c(HID, PA))
train_HID <- subset(trainval$traindata, select=c(HID))
train_labels <- subset(trainval$traindata, select=c(PA))
valid_features <- subset(trainval$validdata, select=-c(HID, PA))
valid_labels <- subset(trainval$validdata, select=c(PA))
valid_HID <- subset(trainval$validdata, select=c(HID))

# prepare data for use in Stan
data <- list(
  K = dim(train_features)[2], # number of features
  Ntr = dim(train_features)[1], # number of training instances
  Nval = dim(valid_features)[1], # number of validation instances
  x = train_features, # training features
  y = train_labels[["PA"]], # training labels
  x_val = valid_features # validation features
)

##### Running Stan model #####
#fit model and get draws
sm <- rstan::stan_model(file = "./bayesian_GLM.stan") # specifying where the Stan model is
model <- rstan::sampling(sm, data=data, seed = random_seed,
                         control = list(adapt_delta = 0.99, max_treedepth = 10)) # run MCMC
saveRDS(model, "bayesGLM_model.rds") # save model so I can recover if R crashes

# load if R crashes
model <- readRDS("bayesGLM_model.rds") 

##### Training and Validation performance #####
library(MLmetrics) # putting this here because my University computer refuses to install this package
draws <- extract(model)
tr_probs <- draws$tr_probs
val_probs <- draws$val_probs
colnames(tr_probs) <- train_HID[["HID"]]
colnames(val_probs) <- valid_HID[["HID"]]

tr_probs_med <- apply(tr_probs, 2, median)
val_probs_med <- apply(val_probs, 2, median)

thres_candidates <- seq(0.01, 0.99, .01)
f1_scores <- sapply(thres_candidates, 
                    function(thres) F1_Score(valid_labels[["PA"]], 
                                             ifelse(val_probs_med >= thres, 1, 0), 
                                             positive = 1))
thres <- thres_candidates[which.max(f1_scores)]

##### Troubleshooting and Tuning #####

# names of columns in draws that contain coefficients and intercept
coeff_names <- c("coeffs[1]","coeffs[2]","coeffs[3]","coeffs[4]","coeffs[5]", 
                 "coeffs[6]","coeffs[7]","coeffs[8]","coeffs[9]","coeffs[10]",
                 "intercept")

# see model statistics in shinystan
my_sso <- launch_shinystan(model)


