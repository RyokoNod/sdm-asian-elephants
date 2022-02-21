library(caret)
library(dplyr)
library(formattable)
library(reliabilitydiag)
library(shinystan)
source("../utils.R")

resultfolder <- '../../data/Results/Bayesian_splineGLM/'

feature_type <- 'SGLM'
normalize <- TRUE 
random_seed = 12244


# Stan diagnostics --------------------------------------------------------

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS("bayessplineGLM_norm_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS("bayessplineGLM_norm_spatialCVfeat_model.rds") 
  }
} else {
  if (feature_type=="GLM"){
    bsplineGLM <- readRDS("bayessplineGLM_randCVfeat_model.rds") 
  }
  if (feature_type=="SGLM"){
    bsplineGLM <- readRDS("bayessplineGLM_spatialCVfeat_model.rds") 
  }
}

# see model statistics in shinystan
my_sso <- launch_shinystan(bsplineGLM)

# Numerical score table ---------------------------------------------------

if (normalize==TRUE){
  evals_file <-  paste('CVresult_matrix_n',feature_type,'_seed', random_seed, '.rds',sep='')
}else{
  evals_file <-  paste('CVresult_matrix_',feature_type,'_seed', random_seed, '.rds',sep='')
}
evals <- readRDS(evals_file)
formattable(evals) # display scores

# Calibration plots -------------------------------------------------------

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