library(rstan)
library(loo)
source("utils.R")
library(shinystan)
#options(mc.cores=parallel::detectCores())  # use all available cores

random_seed = 12244 # set random seed
datafolder <- '../data/Modeling_Data/'
resultfolder <- '../data/Results/Bayesian_SGLM/'

# GLM for random CV feature set, SGLM for spatial CV feature set
feature_type <- 'GLM'

trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')

# present day data on land. Overlaps with training and validation data
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='')

##### Preparing the training, validation, and test data #####

# import the present-day climate variables with Asian elephant presence
traindata_master <- read.csv(trainfile, header=TRUE)

# import the future climate variables
testdata <- read.csv(testfile, header=TRUE)

# do the training and validation split (validation data != test data) 
trainval <- trainvalsplit(traindata_master, 0.8, random_seed=random_seed)
train_features <- subset(trainval$traindata, select=-c(PA))
train_labels <- subset(trainval$traindata, select=c(PA))
valid_features <- subset(trainval$validdata, select=-c(PA))
valid_labels <- subset(trainval$validdata, select=c(PA))

# prepare data for use in Stan
data <- list(
  K = dim(train_features)[2], # number of features
  Ntr = dim(train_features)[1], # number of training instances
  Nval = dim(valid_features)[1], # number of validation instances
  x = train_features, # training features
  y = train_labels[["PA"]], # training labels
  x_val = valid_features # validation features
)

##### Running Stan model #####
#fit model and get draws
sm <- rstan::stan_model(file = "./bayesian_GLM.stan") # specifying where the Stan model is
model <- rstan::sampling(sm, data=data, seed = random_seed, iter = 3000,
                         control = list(adapt_delta = 0.99, max_treedepth = 10)) # run MCMC
saveRDS(model, "bayesGLM_model.rds") # save model so I can recover if R crashes

model <- readRDS("bayesGLM_model.rds") # load if R crashes

draws <- extract(model)

##### Playing around with the results #####

# names of columns in draws that contain coefficients and intercept
coeff_names <- c("coeffs[1]","coeffs[2]","coeffs[3]","coeffs[4]","coeffs[5]", 
                 "coeffs[6]","coeffs[7]","coeffs[8]","coeffs[9]","coeffs[10]",
                 "coeffs[11]","intercept")

# test shinystan
my_sso <- launch_shinystan(model)

# summary of posterior draws 
print(model, pars = coeff_names)

# trace plot
traceplot(model, pars = coeff_names)

# pairs plot, can plot divergences of parameters, though not sure how they're used
# red: divergences, yellow: a transition that hit the maximum treedepth
pairs(model, pars = coeff_names, las = 1)

# plots posteriors of coefficients, red is 80% credible interval, black line is 95%
plot(model, pars=coeff_names)

# show parameters for the chains
sampler_params <- get_sampler_params(model, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)

coeff_idx <- union(grep("^intercept.*",names(draws)),grep("^coeffs.*",names(draws)))
coeff_draws  <- draws[,coeff_idx]

hist(draws[,"coeffs[2]"])
