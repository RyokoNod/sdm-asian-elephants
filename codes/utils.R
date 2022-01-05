trainvalsplit <- function(master_data, train_prop, random_seed=123456){
  # <Overview>
  # Given a dataset, this function separates the data into training
  # and validation set. (validation set != test set)
  # <Parameters>
  # master_data: master data with HID, climate features, and PA labels
  # train_prop: what proportion of data needs to be in training set
  # random_seed: the random seed for the split
  # <Returns>
  # List that contains
  # traindata: the shuffled data to use as training set
  # validdata: the shuffled data to use as validation set
  
  set.seed(random_seed) # set random seed
  
  # I am going to get the same proportion of each label,
  # so doing the train/validation split separately
  PA0 <- subset(master_data, PA==0) # hexagons without Elephas presence
  PA1 <- subset(master_data, PA==1) # hexagons with Elephas presence
  
  # get the shuffled indices
  smp_size_PA0 <- floor(train_prop * nrow(PA0))
  smp_size_PA1 <- floor(train_prop * nrow(PA1))
  train_ind_PA0 <- sample(seq_len(nrow(PA0)), size = smp_size_PA0)
  train_ind_PA1 <- sample(seq_len(nrow(PA1)), size = smp_size_PA1)
  
  # select random
  train_PA0 <- PA0[train_ind_PA0, ]
  valid_PA0 <- PA0[-train_ind_PA0, ]
  train_PA1 <- PA1[train_ind_PA1, ]
  valid_PA1 <- PA1[-train_ind_PA1, ]
  
  # create the training and validation data (not shuffled yet)
  traindata <- rbind(train_PA0, train_PA1)
  validdata <- rbind(valid_PA0, valid_PA1)
  
  # shuffle the rows again
  train_ind <- sample(nrow(traindata))
  valid_ind <- sample(nrow(validdata))
  traindata <- traindata[train_ind, ]
  validdata <- validdata[valid_ind, ]
  
  trainval <- list("traindata"=traindata, "validdata"=validdata)
  return(trainval)
}

bayesGLM_testpred <- function(model, testdata, N, matrixpath, csvpath, seed=123456){
  # <Overview>
  # Creates and saves Bayesian logistic regression predictions for 
  # the given model and input features
  # (distribution, point prediction, interval information)
  # <Parameters>
  # model: The Stan model after inference
  # testdata: The feature file for hexagonal grid (including HID)
  # N: Number of samples per distribution
  # matrixpath: Where you want your distribution matrix saved (file name)
  # csvpath: Where you want your point predictions and interval info saved (file name)
  # seed: Random seed for sampling
  # <Returns>
  # Does not return anything, but saves two output files.
  # The file specified with matrixpath: The distribution matrix as rds
  # The file specified with csvpath: The point predictions and intervals as csv
  
  set.seed(seed)
  draws <- extract(model) # get the sample draws from model
  test_features <- subset(testdata, select=-c(HID)) # get the features from the model
  
  # get the posterior distributions from the fitted model
  intercept_post <- draws$intercept
  coeffs_post <- draws$coeffs
  
  nhex <- dim(test_features)[1] # number of hexagons in test data
  nfeatures <-dim(test_features)[2] # number of features in test data
  
  intercept_samples <- sample(intercept_post, size = N) # get samples for intercept
  
  # get samples for coefficients
  coeff_samples <- matrix(0, N, nfeatures)
  for (i in seq(1:nfeatures)){
    coeff_samples[,i] <- sample(coeffs_post[,i], size = N)
  }
  
  # get the prediction draws
  probs_matrix <- matrix(0, N, nhex)
  for (i in seq(1:N)){
    logreg_line <- rowSums(matrix(rep(coeff_samples[i,],each=nhex),nrow=nhex) * test_features) + intercept_samples[i]
    probs_matrix[i,] <- 1/(1 + exp(-logreg_line))
  }
  saveRDS(probs_matrix, matrixpath) #save the prediction draws
  
  # prepare the output data: median, mean, CI (95%), CI width, standard deviation
  median_probs <- as.data.frame(apply(probs_matrix, 2, median))
  mean_probs <- as.data.frame(apply(probs_matrix, 2, mean))
  cilow_probs <- as.data.frame(apply(probs_matrix, 2, quantile, probs=c(0.025)))
  cihigh_probs <- as.data.frame(apply(probs_matrix, 2, quantile, probs=c(0.975)))
  ci_size <-cihigh_probs - cilow_probs
  sd_probs <- as.data.frame(apply(probs_matrix, 2, sd))
  output_probs <- cbind(subset(testdata,select=HID), median_probs, mean_probs, 
                        cilow_probs, cihigh_probs, ci_size, sd_probs)
  colnames(output_probs) <- c("HID", "median_probs","mean_probs", "cilow", 
                              "cihigh", "cisize", "standarddev")
  write.csv(output_probs, csvpath, row.names=FALSE) # write outputs to CSV
}

bayes_trainval_metrics <- function(model, traindata, valdata, pointtype="mean", thres=NULL){
  # <Overview>
  # Computes precision, recall, accuracy, and AUC for trained Stan model outputs
  # <Parameters>
  # model: The Stan model after inference
  # traindata: The training data, including HID and labels
  # valdata: The validation data, including HID and labels
  # pointtype: The point prediction type. The defauly is mean
  # thres: The threshold for predicting positives. If NULL, chooses one that maximizes F1 score
  # <Returns>
  # A dataframe that includes the precision, recall, accuracy, and AUC for training and validation sets
  
  library(MLmetrics) 
  library(formattable)
  
  draws <- extract(model) # get the sample draws from model
  tr_probs <- draws$tr_probs
  val_probs <- draws$val_probs
  
  train_HID <- subset(traindata, select=c(HID))
  train_labels <- subset(traindata, select=c(PA))
  valid_HID <- subset(valdata, select=c(HID))
  valid_labels <- subset(valdata, select=c(PA))
  
  colnames(tr_probs) <- train_HID[["HID"]]
  colnames(val_probs) <- valid_HID[["HID"]]
  
  tr_probs_point <- apply(tr_probs, 2, pointtype)
  val_probs_point <- apply(val_probs, 2, pointtype)
  
  # if there is no threshold value specified, pick one that mazimizes F1 score
  if (is.null(thres)){
    thres_candidates <- seq(0.01, 0.99, .01)
    f1_scores <- sapply(thres_candidates, 
                        function(thres) F1_Score(valid_labels[["PA"]], 
                                                 ifelse(val_probs_point >= thres, 1, 0), 
                                                 positive = 1))
    thres <- thres_candidates[which.max(f1_scores)]
  }
  
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
  
  evals <- data.frame(Dataset = c("Training", "Validation"),
                      Precision = c(trainprec, valprec),
                      Recall = c(trainrec,  valrec),
                      Accuracy = c(trainacc, valacc),
                      AUC = c(trainauc, valauc))
  return(evals)  
}






