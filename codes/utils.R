
# Functions not in use ----------------------------------------------------

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

bayesGLM_testpred <- function(model, testdata, N, matrixpath, csvpath, bernoulli_draws= FALSE, seed=123456){
  # <Overview>
  # Creates and saves Bayesian logistic regression predictions for 
  # the given Stan model and input features
  # (distribution, point prediction, interval information)
  # <Parameters>
  # model: The Stan model after inference
  # testdata: The feature file for hexagonal grid (including HID)
  # N: Number of samples per distribution
  # matrixpath: Where you want your distribution matrix saved (file name)
  # csvpath: Where you want your point predictions and interval info saved (file name)
  # bernoulli_draws: Setting this to TRUE will makee the draws either 0 or 1
  # seed: Random seed for sampling
  # <Returns>
  # Does not return anything, but saves two output files.
  # The file specified with matrixpath: The distribution matrix as rds
  # The file specified with csvpath: The point predictions and intervals as csv
  
  set.seed(seed)
  draws <- extract(model) # get the sample draws from model
  test_features <- subset(testdata, select=-c(HID)) # get the features from the model
  nhex <- dim(test_features)[1] # number of hexagons in test data
  
  # sample of random row indices
  row_sample_ind <- sample(seq_len(nrow(draws$intercept)), size = N)
  
  # get the posterior distributions from the fitted model
  intercept_post <- draws$intercept
  coeffs_post <- draws$coeffs
  
  # get the random draws of intercept and coefficient
  intercept_samples <- draws$intercept[row_sample_ind]
  coeff_samples <- draws$coeffs[row_sample_ind,]
  
  # get the prediction draws
  preds_matrix <- matrix(0, N, nhex)
  for (i in seq(1:N)){
    logreg_line <- rowSums(matrix(rep(coeff_samples[i,],each=nhex),nrow=nhex) * test_features) + intercept_samples[i]
    if (bernoulli_draws==TRUE){
      preds_matrix[i,] <- rbinom(nhex, 1, 1/(1 + exp(-logreg_line)))
    }else{
      preds_matrix[i,] <- 1/(1 + exp(-logreg_line))
    }
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

bayes_trainval_metrics <- function(model, traindata, valdata, pointtype="mean", thres=NULL){
  # <Overview>
  # Computes precision, recall, accuracy, and AUC for trained Stan model outputs
  # <Parameters>
  # model: The Stan model after inference
  # traindata: The training data, including HID and labels
  # valdata: The validation data, including HID and labels
  # pointtype: The point prediction type. The default is mean
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

bayes_lr_calibration <- function(unnorm_model, norm_model, traindata, validdata, pointtype="mean"){
  # <Overview>
  # Plots calibrations for non-Bayesian logistic regression, Bayesian logistic regression with 
  # unnormalized features, and Bayesian logistic regression with normalized features all in one plot.
  # This is not a multi-purpose function, and can only be used in this specific context.
  # <Parameters>
  # unnorm_model: The Stan model with unnormalized features after inference
  # nnorm_model: The Stan model with normalized features after inference
  # traindata: The training data, including HID and labels
  # validdata: The validation data, including HID and labels
  # pointtype: The point prediction type. The default is mean
  # <Returns>
  # No return value. This function is just for plotting the calibration plot.
  
  valid_labels <- subset(validdata, select=c(PA)) # the labels for the validation data
  
  # get outputs for standard logistic regression
  logreg <- glm(formula=PA ~.,
                data=traindata[ , !(colnames(traindata) %in% c("HID"))],
                family=binomial)
  stlr_valprobs <- predict(logreg,newdata=validdata, type="response")
  
  un_draws <- extract(unnorm_model)
  un_valprob_pts <- apply(un_draws$val_probs, 2, pointtype)
  n_draws <- extract(norm_model)
  n_valprob_pts <- apply(n_draws$val_probs, 2, pointtype)
  
  
  calPlotData<-calibration(factor(valid_labels$PA) ~ standard_logreg + unnormalized_Bayesian + normalized_Bayesian, 
                           data = data.frame(standard_logreg=stlr_valprobs,
                                             unnormalized_Bayesian=un_valprob_pts, 
                                             normalized_Bayesian=n_valprob_pts,
                                             y=factor(valid_labels$PA)), 
                           cuts=10, class="1", auto.key = list(columns = 2))
  ggplot(calPlotData)
  
}


# Functions in use --------------------------------------------------------

TSS <- function(predlbls, truelbls, pos="1", neg="0"){
  # <Overview>
  # Calculates the True Skill Statistic (sensitivity + specificity - 1)
  # <Parameters>
  # predlbls: The predicted binary labels for the dataset (labels, not probabilities)
  # truelbls: The real labels, 0 or 1
  # pos: What label counts as positive in the dataset
  # neg: What label counts as negative in the dataset
  # <Returns>
  # The True Skill Statistic value, sensitivity + specificity - 1
  sens <- sensitivity(as.factor(predlbls), as.factor(truelbls), positive=pos)
  spec <- specificity(as.factor(predlbls), as.factor(truelbls), negative=neg)
  
  return(sens + spec - 1)
}

maxTSS_scores <- function(preds, truelbls, pos="1", neg="0"){
  # <Overview>
  # Returns the maximum possible True Skill Statistic and corresponding
  # sensitivity and specificity values
  # <Parameters>
  # preds: The predicted probabilities for the dataset
  # truelbls: The real labels, 0 or 1
  # pos: What label counts as positive in the dataset
  # neg: What label counts as negative in the dataset
  # <Returns>
  # The True Skill Statistic value, sensitivity + specificity - 1
  thres_candidates <- seq(0.01, 0.99, .01)
  tss_scores <- sapply(thres_candidates, 
                      function(thres) TSS(ifelse(preds >= thres, 1, 0), 
                                          truelbls,
                                          pos=pos, neg=neg))
  
  maxth <- thres_candidates[which.max(tss_scores)]
  predlbls <- ifelse(preds >= maxth, 1, 0)
  
  maxsens <- sensitivity(as.factor(predlbls), as.factor(truelbls), positive=pos)
  maxspec <- specificity(as.factor(predlbls), as.factor(truelbls), negative=neg)
  maxTSS <- TSS(as.factor(predlbls), as.factor(truelbls), pos=pos, neg=neg)
  
  maxscores <- list("TSS"=maxTSS, "sensitivity"=maxsens, "specificity"=maxspec)
  return(maxscores)
  
}


brmsGLM_testpred <- function(model, test_data, matrixpath, csvpath, N=500){
  # <Overview>
  # Creates and saves Bayesian logistic regression predictions for 
  # the given BRMS model and input features
  # (distribution, point prediction, interval information)
  # <Parameters>
  # model: The BRMS model after inference
  # testdata: The feature file for hexagonal grid (including HID)
  # N: Number of samples per distribution
  # matrixpath: Where you want your distribution matrix saved (file name)
  # csvpath: Where you want your point predictions and interval info saved (file name)
  # <Returns>
  # Does not return anything, but saves two output files.
  # The file specified with matrixpath: The distribution matrix as rds
  # The file specified with csvpath: The point predictions and intervals as csv
  
  preds_matrix <- posterior_linpred(model, transform=TRUE, ndraws=N,
                                    newdata=subset(test_data,select=-c(HID)))
  saveRDS(preds_matrix, matrixpath) #save the prediction draws
  
  # prepare the output data: median, mean, CI (95%), IQR, standard deviation
  median_probs <- as.data.frame(apply(preds_matrix, 2, median))
  mean_probs <- as.data.frame(apply(preds_matrix, 2, mean))
  cilow_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.025)))
  cihigh_probs <- as.data.frame(apply(preds_matrix, 2, quantile, probs=c(0.975)))
  iqr_probs <- as.data.frame(apply(preds_matrix, 2, IQR))
  sd_probs <- as.data.frame(apply(preds_matrix, 2, sd))
  output_probs <- cbind(subset(test_data,select=HID), median_probs, mean_probs, 
                        cilow_probs, cihigh_probs, iqr_probs, sd_probs)
  colnames(output_probs) <- c("HID", "median_probs","mean_probs", "cilow", 
                              "cihigh", "IQR", "standarddev")
  write.csv(output_probs, csvpath, row.names=FALSE) # write outputs to CSV
}



condeff_surface <- function(model, traindata, feature1, feature2, avgline=TRUE, 
                            trainpoints=TRUE){
  # <Overview>
  # Plots a conditional effect surface plot in the range of the 
  # training data for the specified features.
  # If avgline=TRUE, draws a dashed line through the average of feature2 so that the plot shows
  # where the surface was cut for the default plot in brms's conditional_effect().
  # <Parameters>
  # model: The BRMS model after inference
  # traindata: The training data dataframe
  # feature1: The feature to be plotted on the x-axis. Specify as character string.
  # feature2: The feature to be plotted on the y-axis. Specify as character string.
  # avgline: Whether or not to draw a horizontal line through the average of feaure2. Logical.
  # trainpoints: Whether of not to plot the training data on the surface. Logical.
  # <Returns>
  # Does not return anything. Just plots the contour plot.
  
  
  names(traindata)[names(traindata) == 'PA'] <- 'labels'
  feature2_mean <- apply(traindata[feature2], 2, mean)
  eff <- paste(feature1, feature2, sep=":")
  
  # break point list for the predicted values
  breaklist <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  
  # if I don't perturb 1s and 0s slightly, ggplot fails to plot them
  cond_surface <- conditional_effects(model, effects=eff, surface=TRUE)
  cond_surface[[eff]]$estimate__[cond_surface[[eff]]$estimate__ == 1] <- 1 - 1e-06  
  cond_surface[[eff]]$estimate__[cond_surface[[eff]]$estimate__ == 0] <- 0 + 1e-06
  
  surface_plot <- ggplot() +
    geom_contour_filled(data=cond_surface[[eff]], 
                        aes(x = effect1__, y = effect2__, z = estimate__),
                        breaks=breaklist) +
    labs(x=feature1, y=feature2, fill="predictions") 
  
  if (trainpoints==TRUE){
    surface_plot <- surface_plot +
      geom_point(data=traindata, aes_string(x=feature1,y= feature2, color="labels"), alpha = 0.5) +
      scale_color_manual(values=c("#33FFFF","#FF66FF"))
  }
  if (avgline==TRUE){
    surface_plot <- surface_plot +
      geom_hline(yintercept =feature2_mean, linetype="dashed", color="red")
  }
  print(surface_plot)
}


test_condeff_surface <- function(model, testdata, feature1, feature2){
  # <Overview>
  # Plots a conditional effect surface plot in the range of the test data
  # (present-day and future combined) for the specified features.
  # <Parameters>
  # model: The BRMS model after inference
  # testdata: The test data dataframe. Be sure to combine the present and future data
  #           outside of this function. This function does not do it for you.
  # feature1: The feature to be plotted on the x-axis. Specify as character string.
  # feature2: The feature to be plotted on the y-axis. Specify as character string.
  # <Returns>
  # A ggplot object of the surface plot that covers the area of the test data.
  
  
  #set the features to be plotted
  eff <- paste(feature1, feature2, sep=":")
  
  # set the range for each feature axis. resolution is 30*30
  maxf1 <- max(testdata[[feature1]])
  minf1 <- min(testdata[[feature1]])
  maxf2 <- max(testdata[[feature2]])
  minf2 <- min(testdata[[feature2]])
  cond <- list(f1 = seq(minf1, maxf1, length.out=30), f2=seq(minf2, maxf2, length.out=30))
  names(cond) <- c(feature1, feature2)
  
  # break point list for the predicted values
  breaklist <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  
  # if I don't perturb 1s and 0s slightly, ggplot fails to plot them
  cond_surface <- conditional_effects(model, effects=eff, int_conditions=cond, surface=TRUE)
  cond_surface[[eff]]$estimate__[cond_surface[[eff]]$estimate__ == 1] <- 1 - 1e-06  
  cond_surface[[eff]]$estimate__[cond_surface[[eff]]$estimate__ == 0] <- 0 + 1e-06
  
  surface_plot <- ggplot() +
    geom_contour_filled(data=cond_surface[[eff]], 
                        aes(x = effect1__, y = effect2__, z = estimate__),
                        breaks=breaklist) +
    xlab(sub("_.*","",feature1)) + ylab(sub("_.*","",feature2))
  
  return(surface_plot)
}

plot_hollow_rec <- function(plotobj, data, feature1, feature, col, al=1.0){
  # <Overview>
  # Plots a hollow rectangle that covers the range of data[feature1] and data[feature2] 
  # on plotobj Intended to be used in combination with test_condeff_surface to plot
  # feature ranges of interest on the surface plot.
  # <Parameters>
  # plotobj: The surface plot returned from test_condeff_surface
  # data: A dataframe of that contains feature1 and feature2 of the area of interest
  # feature1: The feature to be plotted on the x-axis. Specify as character string
  # feature2: The feature to be plotted on the y-axis. Specify as character string
  # col: A string that matches the custom legend specified outside of this function
  # al: The alpha value of the rectangle to be plotted. Default is 1.0
  # <Returns>
  # A surface plot with the feature range data overlaid as a hollow rectangle
  minf1 <- min(data[[feature1]])
  minf2 <- min(data[[feature2]])
  maxf1 <- max(data[[feature1]])
  maxf2 <- max(data[[feature2]])
  
  return_plot <- plotobj +
    geom_segment(aes(x=minf1, y=minf2, xend=minf1, yend=maxf2, color=col), alpha=al) +
    geom_segment(aes(x=minf1, y=maxf2, xend=maxf1, yend=maxf2, color=col), alpha=al) +
    geom_segment(aes(x=maxf1, y=minf2, xend=maxf1, yend=maxf2, color=col), alpha=al) +
    geom_segment(aes(x=minf1, y=minf2, xend=maxf1, yend=minf2, color=col), alpha=al)
  
  return(return_plot)
}

