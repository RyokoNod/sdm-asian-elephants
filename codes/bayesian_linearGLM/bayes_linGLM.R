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
random_seed <- 12244 # set random seed
datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_linearGLM/baseline_priors/'
feature_type <- 'GLM' # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <-FALSE # TRUE if you want to normalize the data
adapt_d <- 0.99
treedepth <- 11

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='') # present day climatic

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)


# Define formula and priors -----------------------------------------------
if (feature_type=="GLM"){
  #formula <- as.factor(PA) ~ 0 + Intercept + BIO03_Mean + TN10P_IDW1N10 +
  #  GSL_IDW1N10 + TNX_IDW1N10 + ID_IDW1N10 + BIO14_Mean + BIO18_Mean +
  #  CWD_IDW1N10 + RX1DAY_IDW1N10 + WSDI_IDW1N10
  formula <-PA ~ 0 + Intercept + BIO03_Mean + TN10P_IDW1N10 +
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
}else{
  #formula <- as.factor(PA) ~ 0 + Intercept + BIO08_Mean + TXX_IDW1N10 +
  #  BIO02_Mean + TN90P_IDW1N10 + ID_IDW1N10 + BIO14_Mean + BIO18_Mean +
  #  CWD_IDW1N10 + RX1DAY_IDW1N10 + WSDI_IDW1N10
  formula <- PA ~ 0 + Intercept + BIO08_Mean + TXX_IDW1N10 +
    BIO02_Mean + TN90P_IDW1N10 + ID_IDW1N10 + BIO14_Mean + BIO18_Mean +
    CWD_IDW1N10 + RX1DAY_IDW1N10 + WSDI_IDW1N10
  priors <- c(set_prior("normal(0,5)", class="b", coef="BIO08_Mean"),
              set_prior("normal(0,5)", class="b", coef="TXX_IDW1N10"),
              set_prior("normal(0,5)", class="b", coef="BIO02_Mean"),
              set_prior("normal(0,5)", class="b", coef="TN90P_IDW1N10"),
              set_prior("normal(0,5)", class="b", coef="ID_IDW1N10"),
              set_prior("normal(0,5)", class="b", coef="BIO14_Mean"),
              set_prior("normal(0,5)", class="b", coef="BIO18_Mean"),
              set_prior("normal(0,5)", class="b", coef="CWD_IDW1N10"),
              set_prior("normal(0,5)", class="b", coef="RX1DAY_IDW1N10"),
              set_prior("normal(0,5)", class="b", coef="WSDI_IDW1N10"),
              set_prior("normal(0,10)", class="b", coef="Intercept"))
}



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
                 control = list(adapt_delta = adapt_d, max_treedepth = treedepth),
                 seed=random_seed
                 )
  
  # looks like brms posterior predictive draws don't let us set random seeds
  # which means the predictions will be slightly different every time
  trainpred_matrix <- brms::posterior_linpred(blinGLM, transform=TRUE, ndraws=500)
  valpred_matrix <- brms::posterior_linpred(blinGLM, transform=TRUE, ndraws = 500, 
                                            newdata=cbind(valdata$features, valdata$labels))
  trainpred <- apply(trainpred_matrix, 2, median)
  valpred <- apply(valpred_matrix, 2, median)
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
  pres_testdata <- cbind(HID=pres_testdata$HID, pres_testfeatures) # put HID together again
}

# create model
blinGLM <- brm(formula=formula, 
               data=cbind(train_features, train_labels),
               family=bernoulli(link="logit"),
               prior=priors,
               control = list(adapt_delta = adapt_d, max_treedepth = treedepth),
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

# predict and save to files
if (normalize==TRUE){
  pres_preds_matrix <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                            '.rds',sep='')
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  pres_preds_matrix <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                            '.rds',sep='')
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
brmsGLM_testpred(blinGLM, pres_testdata, matrixpath=pres_preds_matrix, csvpath=pres_preds_file)


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

# predict and save to files
if (normalize==TRUE){
  preds_matrix <-  paste(resultfolder,'preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                       '.rds',sep='')
  preds_file <-  paste(resultfolder,'preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                            '.csv',sep='')
}else{
  preds_matrix <-  paste(resultfolder,'preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                       '.rds',sep='')
  preds_file <-  paste(resultfolder,'preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                            '.csv',sep='')
}
brmsGLM_testpred(blinGLM, testdata, matrixpath=preds_matrix, csvpath=preds_file)




# Model statistics --------------------------------------------------------

# load model if needed
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



# Calibration plots (validation folds) -------------------------------------------------------

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
ggplot(calPlotData) + 
  labs(x="Predicted probabilities (percentage)", y="Observed event frequencies (percentage)")

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = valpreds_all$valpred, y = valpreds_all$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted probabilities",
       y="Observed event frequencies")+
  bayesplot::theme_default(base_family = "sans")


# Calibration plots (training data, final model) -------------------------------------------------------

# load training predictions
if (normalize==TRUE){
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                            '.csv',sep='')
}else{
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                            '.csv',sep='')
}
pres_preds <- read.csv(pres_preds_file)


# stick the presence/absence together
trainpreds <- filter(pres_preds, HID %in% traindata_master$HID)
trainpreds <- left_join(trainpreds, traindata_master[c("HID", "PA")], by="HID")

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(trainpreds$PA) ~ bayes_GLM, 
                         data = data.frame(bayes_GLM=trainpreds$median_probs,
                                           y=factor(trainpreds$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData) + 
  labs(x="Predicted probabilities (percentage)", y="Observed event frequencies (percentage)")


# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = trainpreds$median_probs, y = trainpreds$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted probabilities",
       y="Observed event frequencies")+
  bayesplot::theme_default(base_family = "sans")


# Conditional effects -----------------------------------------------------

modelfolder <- './response_notfactor/'

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_norm_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_norm_spatialCVfeat_model.rds",sep='')) 
  }
} else {
  if (feature_type=="GLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

# conditional regression line
condreg <- conditional_effects(blinGLM, method="posterior_linpred")
plot(condreg)

# conditional effects
condeffs <- conditional_effects(blinGLM)
plot(condeffs, points=TRUE)

# (Troubleshooting) Create Stan code --------------------------------------

# separate the labels and features from the master training data
train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
train_labels <- subset(traindata_master, select=c(PA))

make_stancode(formula=formula, 
    data=cbind(train_features, train_labels),
    family=bernoulli(link="logit"),
    prior=priors,
    save_model="brms_bayeslinGLM.stan"
)

