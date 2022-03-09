library(brms)
library(caret)
library(dplyr)
library(formattable)
library(reliabilitydiag)
library(shinystan)
source("../utils.R")

modelfolder <- './bnorm_sdst/response_notfactor/'
resultfolder <- '../../data/Results/Bayesian_splineGLM/bnorm_sdst/k_default/'
datafolder <- '../../data/Modeling_Data/'

feature_type <- 'SGLM'
normalize <- FALSE
k <- -1 # the number of basis functions used for the model
random_seed = 12244


# Stan diagnostics --------------------------------------------------------

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_norm_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_norm_spatialCVfeat_model.rds", sep='')) 
  }
} else {
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

# see model statistics in shinystan
my_sso <- launch_shinystan(bsplineGLM)


# Conditional smooths and effects -----------------------------------------------------

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_norm_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_norm_spatialCVfeat_model.rds", sep='')) 
  }
} else {
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS(paste(modelfolder, "bayessplineGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

# splines
conditional_smooths(bsplineGLM)

# conditional effects
condeffs <- conditional_effects(bsplineGLM)
plot(condeffs, points=TRUE)

# Numerical score table ---------------------------------------------------

if (normalize==TRUE){
  evals_file <-  paste('CVresult_matrix_n',feature_type,'_seed', random_seed, '.rds',sep='')
}else{
  evals_file <-  paste('CVresult_matrix_',feature_type,'_seed', random_seed, '.rds',sep='')
}
evals <- readRDS(evals_file)
formattable(evals) # display scores

# Calibration plots (validation folds) -------------------------------------------------------

# load validation predictions
if (normalize==TRUE){
  valpreds_file <-  paste(resultfolder,'valpreds_bayessplineGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  valpreds_file <-  paste(resultfolder,'valpreds_bayessplineGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
valpreds_all <- read.csv(valpreds_file)

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(valpreds_all$PA) ~ bayes_GLM, 
                         data = data.frame(bayes_GLM=valpreds_all$valpred,
                                           y=factor(valpreds_all$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData)

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = valpreds_all$valpred, y = valpreds_all$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted Probabilities",
       y="Conditional event probabilities")+
  bayesplot::theme_default(base_family = "sans")


# Calibration plots (training data, final model) -------------------------------------------------------

# load training predictions
if (normalize==TRUE){
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayessplineGLM_',feature_type,'norm_seed', random_seed, 
                          '.csv',sep='')
}else{
  pres_preds_file <-  paste(resultfolder,'pres_preds_bayessplineGLM_',feature_type,'unnorm_seed', random_seed, 
                          '.csv',sep='')
}
pres_preds <- read.csv(pres_preds_file)

# import the present-day climate variables with Asian elephant presence
trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
traindata_master <- read.csv(trainfile, header=TRUE)

# stick the presence/absence together
trainpreds <- filter(pres_preds, HID %in% traindata_master$HID)
trainpreds <- left_join(trainpreds, traindata_master[c("HID", "PA")], by="HID")

# traditional calibration plot with 10 bins
calPlotData<-calibration(factor(trainpreds$PA) ~ bayes_GLM, 
                         data = data.frame(bayes_GLM=trainpreds$median_probs,
                                           y=factor(trainpreds$PA)), 
                         cuts=10, class="1", auto.key = list(columns = 2))
ggplot(calPlotData)

# the new calibration plot by Dimitriadis et al. 2021
newcalPlot <- reliabilitydiag(EMOS = trainpreds$median_probs, y = trainpreds$PA)
reliabilitydiag::autoplot(newcalPlot)+
  labs(x="Predicted Probabilities",
       y="Conditional event probabilities")+
  bayesplot::theme_default(base_family = "sans")
