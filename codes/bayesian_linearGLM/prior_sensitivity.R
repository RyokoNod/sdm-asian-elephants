library(brms)
library(priorsense)
library(dplyr)
library(formattable)

modelfolder <- './baseline_priors/response_notfactor/'

feature_type <- 'GLM'
normalize <- FALSE

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
sens_table <- arrange(sens$sensitivity, variable)


parameter_names <- variables(model)
sens_table$variable <- parameter_names[1:11]
formattable(sens_table)

