library(caret)
library(rstan)
library(brms)

random_seed = 12244 # set random seed
datafolder <- '../../../data/Modeling_Data/'
resultfolder <- './'
feature_type <- 'GLM' # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <- FALSE # TRUE if you want to normalize the data



# Draw functions -----------------------------------------------------------
brmsGLM_testpred1 <- function(model, testdata, matrixpath, csvpath, N=100){
  
  # the function using posterior_linpred
  preds_matrix <- posterior_linpred(model, transform=TRUE, ndraws=N,
                                    newdata=subset(testdata,select=-c(HID)))
  saveRDS(preds_matrix, matrixpath) #save the prediction draws
  
  # prepare the output data: median, mean, CI (95%), CI width, standard deviation
  median_probs <- as.data.frame(apply(preds_matrix, 2, median))
  mean_probs <- as.data.frame(apply(preds_matrix, 2, mean))
  cilow_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.025)))
  cihigh_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.975)))
  ci_size <-cihigh_probs - cilow_probs
  sd_probs <- as.data.frame(apply(preds_matrix, 2, sd))
  output_probs <- cbind(subset(testdata,select=HID), median_probs, mean_probs, 
                        cilow_probs, cihigh_probs, ci_size, sd_probs)
  colnames(output_probs) <- c("HID", "median_probs","mean_probs", "cilow", 
                              "cihigh", "cisize", "standarddev")
  write.csv(output_probs, csvpath, row.names=FALSE) # write outputs to CSV
}

brmsGLM_testpred2 <- function(model, testdata, matrixpath, csvpath, N=100){
  # this one is self-implemented. It predicts from separate individual
  # draws for each feature. It turned out to be faulty.
  draws <- extract(model$fit) # get the sample draws from model
  test_features <- subset(testdata, select=-c(HID)) # get the features from the model
  
  # get the posterior distributions from the fitted model
  intercept_post <- draws$b[,1]
  coeffs_post <- draws$b[,2:11]
  
  nhex <- dim(test_features)[1] # number of hexagons in test data
  nfeatures <-dim(test_features)[2] # number of features in test data
  
  intercept_samples <- sample(intercept_post, size = N) # get samples for intercept
  
  # get samples for coefficients
  coeff_samples <- matrix(0, N, nfeatures)
  for (i in seq(1:nfeatures)){
    coeff_samples[,i] <- sample(coeffs_post[,i], size = N)
  }
  
  # get the prediction draws
  preds_matrix <- matrix(0, N, nhex)
  for (i in seq(1:N)){
    logreg_line <- rowSums(matrix(rep(coeff_samples[i,],each=nhex),nrow=nhex) * test_features) + intercept_samples[i]
    preds_matrix[i,] <- 1/(1 + exp(-logreg_line))
  }
  saveRDS(preds_matrix, matrixpath) #save the prediction draws
  
  # prepare the output data: median, mean, CI (95%), CI width, standard deviation
  median_probs <- as.data.frame(apply(preds_matrix, 2, median))
  mean_probs <- as.data.frame(apply(preds_matrix, 2, mean))
  cilow_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.025)))
  cihigh_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.975)))
  ci_size <-cihigh_probs - cilow_probs
  sd_probs <- as.data.frame(apply(preds_matrix, 2, sd))
  output_probs <- cbind(subset(testdata,select=HID), median_probs, mean_probs, 
                        cilow_probs, cihigh_probs, ci_size, sd_probs)
  colnames(output_probs) <- c("HID", "median_probs","mean_probs", "cilow", 
                              "cihigh", "cisize", "standarddev")
  write.csv(output_probs, csvpath, row.names=FALSE) # write outputs to CSV
}

brmsGLM_testpred3 <- function(model, testdata, matrixpath, csvpath, N=100){
  # this one is self-implemented with independent draws of rows
  # for the posterior distribution matrix. This turned out to be the correct implementation
  draws <- extract(model$fit) # get the sample draws from model
  test_features <- subset(testdata, select=-c(HID)) # get the features from the model
  
  row_sample_ind <- sample(seq_len(nrow(draws$b)), size = N)
  
  nhex <- dim(test_features)[1] # number of hexagons in test data
  
  intercept_samples <- draws$b[row_sample_ind, 1]
  coeff_samples <- draws$b[row_sample_ind, 2:11]
  
  # get the prediction draws
  preds_matrix <- matrix(0, N, nhex)
  for (i in seq(1:N)){
    logreg_line <- rowSums(matrix(rep(coeff_samples[i,],each=nhex),nrow=nhex) * test_features) + intercept_samples[i]
    preds_matrix[i,] <- 1/(1 + exp(-logreg_line))
  }
  saveRDS(preds_matrix, matrixpath) #save the prediction draws
  
  # prepare the output data: median, mean, CI (95%), CI width, standard deviation
  median_probs <- as.data.frame(apply(preds_matrix, 2, median))
  mean_probs <- as.data.frame(apply(preds_matrix, 2, mean))
  cilow_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.025)))
  cihigh_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.975)))
  ci_size <-cihigh_probs - cilow_probs
  sd_probs <- as.data.frame(apply(preds_matrix, 2, sd))
  output_probs <- cbind(subset(testdata,select=HID), median_probs, mean_probs, 
                        cilow_probs, cihigh_probs, ci_size, sd_probs)
  colnames(output_probs) <- c("HID", "median_probs","mean_probs", "cilow", 
                              "cihigh", "cisize", "standarddev")
  write.csv(output_probs, csvpath, row.names=FALSE) # write outputs to CSV
}

# Importing data and models -----------------------------------------------

# specify file names for data
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='')

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)

# separate the HIDs, labels, and the features
train_features <- subset(trainval$traindata, select=-c(HID, PA, Folds))
train_HID <- subset(trainval$traindata, select=c(HID))
train_labels <- subset(trainval$traindata, select=c(PA))

# if specified, normalize the data
if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  valid_features <- predict(preProc, valid_features)
}

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    model <- readRDS("../bayeslinGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    model <- readRDS("../bayeslinGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    model <- readRDS("../bayeslinGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    model <- readRDS("../bayeslinGLM_spatialCVfeat_model.rds") 
  }
}


# Future predictions ------------------------------------------------------

testdata <- read.csv(testfile, header=TRUE) # import the future climate variables

# if specified, normalize data
if (normalize==TRUE){
  test_HID <- subset(testdata, select=HID)
  testdata <- predict(preProc, testdata[,2:dim(testdata)[2]])
  testdata <- cbind(test_HID,testdata)
}

# prediction function 1
if (normalize==TRUE){
  test_matrixpath <- paste(resultfolder,'brms_bayesGLM_pred_norm_',feature_type,'_',random_seed,'_probdraws1.rds',sep='')
  test_csvpath <- paste(resultfolder,'brms_results_norm_',feature_type,'_', random_seed, '_probdraws1.csv',sep='')
}else{
  test_matrixpath <- paste(resultfolder,'brms_bayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_probdraws1.rds',sep='')
  test_csvpath <- paste(resultfolder,'brms_results_unnorm_',feature_type,'_', random_seed, '_probdraws1.csv',sep='')
}
brmsGLM_testpred1(model=model, testdata=testdata, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath)

# prediction function 2
if (normalize==TRUE){
  test_matrixpath <- paste(resultfolder,'brms_bayesGLM_pred_norm_',feature_type,'_',random_seed,'_probdraws2.rds',sep='')
  test_csvpath <- paste(resultfolder,'brms_results_norm_',feature_type,'_', random_seed, '_probdraws2.csv',sep='')
}else{
  test_matrixpath <- paste(resultfolder,'brms_bayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_probdraws2.rds',sep='')
  test_csvpath <- paste(resultfolder,'brms_results_unnorm_',feature_type,'_', random_seed, '_probdraws2.csv',sep='')
}
brmsGLM_testpred2(model=model, testdata=testdata, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath)

# prediction function 3
if (normalize==TRUE){
  test_matrixpath <- paste(resultfolder,'brms_bayesGLM_pred_norm_',feature_type,'_',random_seed,'_probdraws3.rds',sep='')
  test_csvpath <- paste(resultfolder,'brms_results_norm_',feature_type,'_', random_seed, '_probdraws3.csv',sep='')
}else{
  test_matrixpath <- paste(resultfolder,'brms_bayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_probdraws3.rds',sep='')
  test_csvpath <- paste(resultfolder,'brms_results_unnorm_',feature_type,'_', random_seed, '_probdraws3.csv',sep='')
}
brmsGLM_testpred3(model=model, testdata=testdata, 
                  matrixpath=test_matrixpath, csvpath=test_csvpath)


# Present-day predictions -------------------------------------------------

pres_testdata <- read.csv(pres_testfile, header=TRUE) 

# if specified, normalize data
if (normalize==TRUE){
  prestest_HID <- subset(pres_testdata, select=HID)
  pres_testdata <- predict(preProc, pres_testdata[,2:dim(pres_testdata)[2]])
  pres_testdata <- cbind(test_HID,pres_testdata)
}

# prediction function 1
if (normalize==TRUE){
  pres_test_matrixpath <- paste(resultfolder,'brms_presbayesGLM_pred_norm_',feature_type,'_',random_seed,'_probdraws1.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'brms_results_pres_norm_',feature_type,'_', random_seed, '_probdraws1.csv',sep='')
}else{
  pres_test_matrixpath <- paste(resultfolder,'brms_presbayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_probdraws1.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'brms_results_pres_unnorm_',feature_type,'_', random_seed, '_probdraws1.csv',sep='')
}
brmsGLM_testpred1(model=model, testdata=pres_testdata, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath)

# prediction function 2
if (normalize==TRUE){
  pres_test_matrixpath <- paste(resultfolder,'brms_presbayesGLM_pred_norm_',feature_type,'_',random_seed,'_probdraws2.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'brms_results_pres_norm_',feature_type,'_', random_seed, '_probdraws2.csv',sep='')
}else{
  pres_test_matrixpath <- paste(resultfolder,'brms_presbayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_probdraws2.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'brms_results_pres_unnorm_',feature_type,'_', random_seed, '_probdraws2.csv',sep='')
}
brmsGLM_testpred2(model=model, testdata=pres_testdata, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath)

# prediction function 3
if (normalize==TRUE){
  pres_test_matrixpath <- paste(resultfolder,'brms_presbayesGLM_pred_norm_',feature_type,'_',random_seed,'_probdraws3.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'brms_results_pres_norm_',feature_type,'_', random_seed, '_probdraws3.csv',sep='')
}else{
  pres_test_matrixpath <- paste(resultfolder,'brms_presbayesGLM_pred_unnorm_',feature_type,'_',random_seed,'_probdraws3.rds',sep='')
  pres_test_csvpath <- paste(resultfolder,'brms_results_pres_unnorm_',feature_type,'_', random_seed, '_probdraws3.csv',sep='')
}
brmsGLM_testpred3(model=model, testdata=pres_testdata, 
                  matrixpath=pres_test_matrixpath, csvpath=pres_test_csvpath)


