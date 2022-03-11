library(brms)
library(priorsense)
library(dplyr)
library(formattable)

modelfolder <- './'

feature_type <- 'SGLM'
normalize <- TRUE
k <- -1 # the number of basis functions used for the model

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    model <- readRDS(paste(modelfolder, "bayeslinGLM_norm_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    model <- readRDS(paste(modelfolder, "bayeslinGLM_norm_spatialCVfeat_model.rds", sep='')) 
  }
} else {
  if (feature_type=="GLM"){
    model <- readRDS(paste(modelfolder, "bayeslinGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    model <- readRDS(paste(modelfolder, "bayeslinGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

sens <- powerscale_sensitivity(model)
formattable(arrange(sens$sensitivity, variable))



