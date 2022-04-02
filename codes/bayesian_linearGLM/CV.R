library(caret)
library(brms)
library(MLmetrics)
library(dplyr)
source("../utils.R")
options(mc.cores=parallel::detectCores())  # use all available cores

datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_linearGLM/baseline_priors/'
CVmodelfolder <- './CV_intermediate_models/'

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
  print(paste("***************** Fold ", f, " *****************"))
  
  CV_modelname <- paste(CVmodelfolder, 'model', f, '.rds', sep='') # intermediate model name
  
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
  
  print("Getting evaluations...")
  
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

print("***************** Finished CV *****************")
print("Writing validation predictions...")

valpreds_all <- merge(subset(traindata_master, select=c(HID, PA)), valpreds_all, by="HID")
if (normalize==TRUE){
  valpreds_file <-  paste(resultfolder,'valpreds_bayeslinGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  valpreds_file <-  paste(resultfolder,'valpreds_bayeslinGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
write.csv(valpreds_all, valpreds_file, row.names=FALSE)

print("Getting matrix of eval metrics...")

# the final evaluation scores are the mean of the CV folds
evals <- t(data.frame(apply(train_evals, 2, mean), apply(val_evals, 2, mean)))
evals <- data.frame(evals) %>% mutate_if(is.numeric, round, digits=3)
row.names(evals) <- c("Training", "Validation")

if (normalize==TRUE){
  evals_file <-  paste('CVresult_matrix_n',feature_type,'_seed', random_seed, '.rds',sep='')

}else{
  evals_file <-  paste('CVresult_matrix_',feature_type,'_seed', random_seed, '.rds',sep='')
}

saveRDS(evals, evals_file)
print("Done!")