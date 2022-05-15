
# relabeled Shinystan posteriors ------------------------------------------

library(shinystan)

plot_shinystan_posteriors <- function(p, renamed_params, title){
  
  p$data$params <- renamed_params
  
  ggplot(data = p$data) +
    geom_segment(aes(x = ll, y = params, xend = hh, yend = params)) +
    geom_segment(aes(x = l, y = params, xend = h, yend = params), size=2, color="#D55E00") +
    geom_point(aes(x=m, y=params),shape=21, size=3, stroke=0.5, color="#D55E00", fill = "#56B4E9") +
    labs(title=title) +
    ylab("Parameters") + xlab("")
}

#shinystan_output_folder <- "./bayesian_linearGLM/adjusted_priors/"
#shinystan_output_file <- "bayeslinGLM_randCVfeat_posteriors_adjpriors.RData"
shinystan_output_folder <- "./bayesian_splineGLM/bnorm_sdsnorm/k1/"
shinystan_output_file <- "bayessplineGLM_randCVfeat_posteriors_norm-norm-k1.RData"

load(paste(shinystan_output_folder, shinystan_output_file, sep=""))

#shinystan_multiparam_gg$data <- shinystan_multiparam_gg$data[-1,]


#plot_title <- "Raw random-CV features/ adjusted priors"
plot_title <- "Raw random-CV features/ adjusted priors and basis dimension"

#renamed_params <- c("Coeff BIO03",
#                    "Coeff TN10P", "Coeff GSL",
#                    "Coeff TNX", "Coeff ID",
#                    "Coeff BIO14", "Coeff BIO18",
#                    "Coeff CWD", "Coeff RX1DAY",
#                    "Coeff WSDI")
renamed_params <- c("Intercept", "Coeff BIO03", "Coeff TN10P", "Coeff GSL",
                    "Coeff TNX", "Coeff ID", "Coeff BIO14", "Coeff BIO18",
                    "Coeff CWD", "Coeff RX1DAY", "Coeff WSDI",
                    "NL BIO03", "NL TN10P", "NL GSL",
                    "NL TNX", "NL ID", "NL BIO14", "NL BIO18",
                    "NL CWD", "NL RX1DAY", "NL WSDI")

plot_shinystan_posteriors(shinystan_multiparam_gg, renamed_params, plot_title)

# relabeled conditional effects, Bayesian GLM  -------------------------------------------

library(brms)
library(ggplot2)

# settings
random_seed <- 12244 # set random seed
feature_type <- 'GLM' # GLM for random CV feature set, SGLM for spatial CV feature set
normalize <-TRUE # TRUE if you want to normalize the data

modelfolder <- './bayesian_linearGLM/adjusted_priors/'

# load model
if (normalize==TRUE){
  if (feature_type=="GLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_norm_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_norm_spatialCVfeat_model.rds",sep='')) 
  }
} else {
  if (feature_type=="GLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    blinGLM <- readRDS(paste(modelfolder, "bayeslinGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

# conditional regression line
condreg <- conditional_effects(blinGLM, method="posterior_linpred")

ggplot(data=condreg$BIO14_Mean) +
  geom_ribbon(aes(x=effect1__, ymin = lower__, ymax = upper__), alpha = 0.1) +
  geom_line(aes(x=effect1__, y=estimate__), color="blue") +
  xlab("scaled BIO14") + ylab("log-odds")



