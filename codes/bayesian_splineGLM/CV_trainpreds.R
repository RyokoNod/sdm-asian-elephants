# this script gives training predictions for finished CV models
library(caret)
library(dplyr)

datafolder <- '../../data/Modeling_Data/'
resultfolder <- '../../data/Results/Bayesian_splineGLM/'
CVmodelfolder <- './CV_intermediate_models/'

args = commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==3) {
  args[4] = 12244
}

feature_type <- args[1] # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- as.logical(args[2]) # TRUE if you want to specify model for normalized features
k <- as.integer(args[3]) # the k you used in the brms formula. It's weird but the predictions need this
random_seed = as.integer(args[4]) # random seed used in model name

print(paste("feature type: ", feature_type))
print(paste("normalize: ", normalize))
print(paste("k: ", k))
print(paste("random seed: ", random_seed))

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

nfolds <- length(unique(traindata_master$Folds))

# where to put intermediate files
if (normalize==TRUE){
  CVmodelfolder <-  paste(CVmodelfolder,'normalized_features/',feature_type,'/',sep='')
}else{
  CVmodelfolder <-  paste(CVmodelfolder,'unnormalized_features/',feature_type,'/',sep='')
}

trainpreds_all <- subset(traindata_master, select=c(HID))

for (f in 1:nfolds){
  print(paste("***************** Fold ", f, " *****************"))
  
  CV_modelname <- paste(CVmodelfolder, 'model', f, '.rds', sep='') # intermediate model name
  
  # separate training data for the fold
  fold_train <- subset(traindata_master, Folds!=f)
  
  # separate the HIDs, labels, and the features
  traindata <- list("features"=subset(fold_train, select=-c(HID, PA, Folds)),
                    "labels"=subset(fold_train, select=c(PA)),
                    "HID"= subset(fold_train, select=c(HID)))
  
  # if specified, normalize the data
  if (normalize==TRUE){
    preProc <- preProcess(traindata$features, method=c("range"))
    traindata$features <- predict(preProc, traindata$features)
  }
  
  bsplineGLM <- readRDS(CV_modelname) # read model
  
  print("Getting evaluations...")
  
  # looks like brms posterior predictive draws don't let us set random seeds
  # which means the predictions will be slightly different every time
  trainpred_matrix <- brms::posterior_linpred(bsplineGLM, transform=TRUE, ndraws=500)
  trainpred <- apply(trainpred_matrix, 2, median,  na.rm=TRUE)
  trainpreds_all <- merge(trainpreds_all, cbind(HID=traindata$HID$HID, trainpred=trainpred), by="HID", all=TRUE)
  colnames(trainpreds_all)[colnames(trainpreds_all) == "trainpred"] <- paste("trainpred",f, sep='')
}

# add the mean of the folds at the end
trainpreds_all$trainpreds_mean <- rowMeans(select(trainpreds_all,contains("trainpred")), na.rm=TRUE)

print("Got the training predictions")
print("Writing the predictions...")

if (normalize==TRUE){
  trainpreds_file <-  paste(resultfolder,'trainpreds_bayessplineGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  trainpreds_file <-  paste(resultfolder,'trainpreds_bayessplineGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
write.csv(trainpreds_all, trainpreds_file, row.names=FALSE)