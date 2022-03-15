library(brms)
library(priorsense)
library(dplyr)
library(formattable)

modelfolder <- './bnorm_sdst/k_default/response_notfactor/'

feature_type <- 'GLM'
normalize <- TRUE
k <- -1 # the k you used in the brms formula. It's weird but the predictions need this

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    model <- readRDS(paste(modelfolder, "bayessplineGLM_norm_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    model <- readRDS(paste(modelfolder, "bayessplineGLM_norm_spatialCVfeat_model.rds", sep='')) 
  }
} else {
  if (feature_type=="GLM"){
    model <- readRDS(paste(modelfolder, "bayessplineGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    model <- readRDS(paste(modelfolder, "bayessplineGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

sens <- powerscale_sensitivity(model, component="prior")
formattable(arrange(sens$sensitivity, variable))



