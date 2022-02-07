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
resultfolder <- '../../data/Results/Bayesian_GLM/'
feature_type <- 'GLM' # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- FALSE # TRUE if you want to normalize the data

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='') # present day climatic

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# define formula and priors
formula <- as.factor(PA) ~ 0 + Intercept + BIO03_Mean + TN10P_IDW1N10 +
  GSL_IDW1N10 + TNX_IDW1N10 + ID_IDW1N10 + BIO14_Mean + BIO18_Mean +
  CWD_IDW1N10 + RX1DAY_IDW1N10 + WSDI_IDW1N10
priors <- c(set_prior("normal(0,5)", class="b", coef="BIO03_Mean"),
            set_prior("normal(0,5)", class="b", coef="TN10P_IDW1N10"),
            set_prior("normal(0,5)", class="b", coef="GSL_IDW1N10"),
            set_prior("normal(0,5)", class="b", coef="TNX_IDW1N10"),
            set_prior("normal(0,5)", class="b", coef="ID_IDW1N10"),
            set_prior("normal(0,5)", class="b", coef="BIO14_Mean"),
            set_prior("normal(0,5)", class="b", coef="BIO18_Mean"),
            set_prior("normal(0,5)", class="b", coef="CWD_IDW1N10"),
            set_prior("normal(0,5)", class="b", coef="RX1DAY_IDW1N10"),
            set_prior("normal(0,5)", class="b", coef="WSDI_IDW1N10"),
            set_prior("normal(0,10)", class="b", coef="Intercept"))



# Spatial cross validation ------------------------------------------------

nfolds <- length(unique(traindata_master$Folds))
eval_metrics <- c("AUC", "TSS", "sensitivity", "specificity") # evaluation metric list

# create matrices to store evaluations for each fold
train_evals <- matrix(nrow=nfolds, ncol=length(eval_metrics))
val_evals <- matrix(nrow=nfolds, ncol=length(eval_metrics))
colnames(train_evals)<- eval_metrics
colnames(val_evals)<- eval_metrics

valpreds_all <- data.frame() # an empty data frame to store validation predictions


for (f in 1:nfolds){
  # separate training data and validation data for the fold
  fold_train <- subset(traindata_master, Folds!=f)
  fold_val <- subset(traindata_master, Folds==f)
  
  # separate the HIDs, labels, and the features
  traindata <- list("features"=subset(fold_train, select=-c(HID, PA, Folds)),
                    "labels"=subset(fold_train, select=c(PA)),
                    "HID"= subset(fold_train, select=c(HID)))
  valdata <- list("features"=subset(fold_val, select=-c(HID, PA, Folds)),
                    "labels"=subset(fold_val, select=c(PA)),
                    "HID"= subset(fold_val, select=c(HID)))
  
  # if specified, normalize the data
  if (normalize==TRUE){
    preProc <- preProcess(traindata$features, method=c("range"))
    traindata$features <- predict(preProc, traindata$features)
    valdata$features <- predict(preProc,  valdata$features)
  }
  
  blinGLM <- brm(formula=formula, 
                 data=cbind(traindata$features, traindata$labels),
                 family=bernoulli(link="logit"),
                 prior=priors,
                 control = list(adapt_delta = 0.99, max_treedepth = 11),
                 seed=random_seed
                 )
  
  # looks like brms posterior predictive draws don't let us set random seeds
  # which means the predictions will be slightly different every time
  trainpred <- predict(blinGLM)[,1]
  valpred <- predict(blinGLM, newdata=cbind(valdata$features, valdata$labels))[,1]
  valpreds_all <- rbind(valpreds_all, cbind(HID=valdata$HID$HID, valpred=valpred))
  
  # calculate AUC for the fold and store
  train_evals[f, "AUC"] <- AUC(trainpred, traindata$labels$PA)
  val_evals[f, "AUC"] <- AUC(valpred, valdata$labels$PA)
  
  # calculate TSS, sensitivity, specificity for the fold and store
  train_TSSscores <- maxTSS_scores(trainpred, traindata$labels$PA)
  val_TSSscores <- maxTSS_scores(valpred, valdata$labels$PA)
  train_evals[f, "TSS"] <- train_TSSscores$TSS
  val_evals[f, "TSS"] <- val_TSSscores$TSS
  train_evals[f, "sensitivity"] <- train_TSSscores$sensitivity
  val_evals[f, "sensitivity"] <- val_TSSscores$sensitivity
  train_evals[f, "specificity"] <- train_TSSscores$specificity
  val_evals[f, "specificity"] <- val_TSSscores$specificity
}

# Save the validation predictions because RStudio tends to crash after this point
# I will use these later for calibration plots
valpreds_all <- merge(subset(traindata_master, select=c(HID, PA)), valpreds_all, by="HID")

if (normalize==TRUE){
  valpreds_file <-  paste(resultfolder,'valpreds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  valpreds_file <-  paste(resultfolder,'valpreds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
write.csv(valpreds_all, valpreds_file, row.names=FALSE)

# the final evaluation scores are the mean of the CV folds
evals <- t(data.frame(apply(train_evals, 2, mean), apply(val_evals, 2, mean)))
evals <- data.frame(evals) %>% mutate_if(is.numeric, round, digits=3)
row.names(evals) <- c("Training", "Validation")

formattable(evals) # display scores


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
}

# create model
blinGLM <- brm(formula=formula, 
               data=cbind(train_features, train_labels),
               family=bernoulli(link="logit"),
               prior=priors,
               control = list(adapt_delta = 0.99, max_treedepth = 11),
               seed=random_seed
)

# save model so I can recover if R crashes
if (normalize==TRUE){
  if (feature_type=="GLM"){
    saveRDS(blinGLM, "bayeslinGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    saveRDS(blinGLM, "bayeslinGLM_norm_spatialCVfeat_model.rds")
  }
} else{
  if (feature_type=="GLM"){
    saveRDS(blinGLM, "bayeslinGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    saveRDS(blinGLM, "bayeslinGLM_spatialCVfeat_model.rds")
  }
}

# load if R crashes
if (normalize==TRUE){
  if (feature_type=="GLM"){
    blinGLM <- readRDS("bayeslinGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS("bayeslinGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    blinGLM <- readRDS("bayeslinGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS("bayeslinGLM_spatialCVfeat_model.rds") 
  }
}

# get predictions for present-day climate
pres_testpred <- predict(blinGLM, newdata=pres_testfeatures)
pres_testoutputs <- cbind(HID=pres_testdata$HID,mean_pred=pres_testpred[,1], sd=pres_testpred[,2])

# save predictions to file
if (normalize==TRUE){
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
write.csv(pres_testoutputs, pres_preds_file, row.names=FALSE)




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
}


# load if R crashes
if (normalize==TRUE){
  if (feature_type=="GLM"){
    blinGLM <- readRDS("bayeslinGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS("bayeslinGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    blinGLM <- readRDS("bayeslinGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS("bayeslinGLM_spatialCVfeat_model.rds") 
  }
}

# get the predictions for future climate
testpred <- predict(blinGLM, newdata=testfeatures)
testoutputs <- cbind(HID=testdata$HID,mean_pred=testpred[,1], sd=testpred[,2])

# save predictions to file
if (normalize==TRUE){
  preds_file <-  paste(resultfolder,'preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                            '.csv',sep='')
}else{
  preds_file <-  paste(resultfolder,'preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                            '.csv',sep='')
}
write.csv(testoutputs, preds_file, row.names=FALSE)



# Model statistics --------------------------------------------------------

# load if R crashes
if (normalize==TRUE){
  if (feature_type=="GLM"){
    blinGLM <- readRDS("bayeslinGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS("bayeslinGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    blinGLM <- readRDS("bayeslinGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS("bayeslinGLM_spatialCVfeat_model.rds") 
  }
}

# see model statistics in shinystan
my_sso <- launch_shinystan(blinGLM)



# Calibration plots -------------------------------------------------------

# load in case RStudio crashed
if (normalize==TRUE){
  valpreds_file <-  paste(resultfolder,'valpreds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  valpreds_file <-  paste(resultfolder,'valpreds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
valpreds_all <- read.csv(valpreds_file)

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(valpreds_all$PA) ~ bayes_linGLM, 
                         data = data.frame(bayes_linGLM=valpreds_all$valpred,
                                           y=factor(valpreds_all$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData)

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = valpreds_all$valpred, y = valpreds_all$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted Probabilities",
       y="Conditional event probabilities")+
  bayesplot::theme_default(base_family = "sans")





