#libraries
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library("rstan")
library("loo")

#read dataset as list
filepath <- 'Dataset_spine.csv'
spine <- read.csv(file = filepath)

##### Formatting Data #####
#remove the last column, it only contains definitions
spine <- spine[-14] 

#change the column names so we can understand them
colnames(spine) <- c("pelvic_incidence","pelvic_tilt",
                     "lumbar_lordosis_angle","sacral_slope",
                     "pelvic_radius","degree_spondylolisthesis",
                     "pelvic_slope","direct_tilt",
                     "thoracic_slope","cervical_tilt",
                     "sacrum_angle","scoliosis_slope",
                     "label")

#set index for training and test set
tr_idx <- sample(1:nrow(spine), 0.8 * nrow(spine))
ts_idx <- setdiff(1:nrow(spine), tr_idx)
?sample()

#separate features and labels for use in Stan
spine_features <- subset(spine, select=-c(label))
spine_labels <- subset(spine,select=c(label))

#change notation of labels: Abnormal=1, Normal=0
spine_labels <- lapply(spine_labels, function(ch) gsub("Abnormal", 1, ch))
spine_labels <- lapply(spine_labels, function(ch) gsub("Normal", 0, ch))
spine_labels <- as.numeric(unlist(spine_labels))

#create training and test set
#training set
features_tr <- spine_features[tr_idx,]
labels_tr <- spine_labels[tr_idx]
#test set
features_ts <- spine_features[ts_idx,]
labels_ts <- spine_labels[ts_idx]

#prepare data to use in Stan
#these are for all the features
data_allfeatures <- list(
  K = dim(features_tr)[2],
  N_tr = dim(features_tr)[1],
  N_ts = dim(features_ts)[1],
  x = features_tr,
  y = labels_tr,
  x_ts = features_ts
)

##### Extra: Looking at the Data #####
#getting the dataframe for ggplot
spinedf <- as.data.frame(spine)

#restructuring the dataframe for plotting
long = melt(spinedf)

#plot density by normal/abnormal
long %>%
  ggplot(aes(x=value, fill=label)) + 
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales='free', ncol = 3) + 
  labs(x = NULL, y = NULL) + 
  theme(text = element_text(size=18))

#plot density without separating into normal/abnormal
long %>%
  ggplot(aes(x=value, fill=variable)) + 
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales='free', ncol = 3) + 
  labs(x = NULL, y = NULL) + 
  theme_minimal()

##### Utility Function #####

#Input1: The bernoulli MCMC draws of Stan output
#Input2: The threshold for input being positive (default is majority vote)
#Output: List of classification results
util <- function(pred_draws, th=0.5){
  pred <- c() #the predictions to return
  
  #if the proportion of positive (ones) in the MCMC draws
  #is greater than the threshold, classify that data point as positive
  for (i in  1:dim(pred_draws)[2]){
    prop <- sum(pred_draws[,i]) / length(pred_draws[,i])
    if (prop > th){
      pred[i] <- 1
    }
    else{
      pred[i] <- 0
    }
  }
  return(pred)
}

##### Testing a Stan Model #####
#only using 2 features for speed
#if you need all the features, there is a code for it above
features_tr_short <- subset(features_tr, select=c(pelvic_tilt,degree_spondylolisthesis))
features_ts_short <- subset(features_ts, select=c(pelvic_tilt,degree_spondylolisthesis))

data_shortfeatures <- list(
  K = dim(features_tr_short)[2],
  N_tr = dim(features_tr_short)[1],
  N_ts = dim(features_ts_short)[1],
  x = features_tr_short,
  y = labels_tr,
  x_ts = features_ts_short
)

#fit model and get draws
sm <- rstan::stan_model(file = "backpain_ver2.stan")
model <- rstan::sampling(sm, data=data_shortfeatures)
draws <- as.matrix(model)

#if you want to look at Rhat values use this:
monitor(model)[1:3,"Rhat"] #adjust index as necessary

hist(draws[,"beta[2]"])

#looking at loo results
model_loo <- loo(draws[,1:3]) #adjust index as necessary
plot(model_loo) #plot k-values
pareto_k_influence_values(model_loo) #get the specific k-values
model_loo #loo result

#extract the test set and training set
colnames(draws) #use this to visually inspect column indices
trpred_draws <- draws[, 4:251]
tspred_draws <- draws[, 252:313]

#accuracy of predictions
sum(util(trpred_draws) == labels_tr) / length(labels_tr) #training error
sum(util(tspred_draws) == labels_ts) / length(labels_ts) #test error

#plot for report
hist(tspred_draws[,3],
     main="Example of an input for utility function",
     xlab="Binomial draws of prediction results",
     cex=2)
