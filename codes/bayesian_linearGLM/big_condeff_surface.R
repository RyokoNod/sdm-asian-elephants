library(brms)
library(caret)
library(dplyr)
library(formattable)
library(reliabilitydiag)
library(shinystan)
source("../utils.R")

modelfolder <- './adjusted_priors/'
resultfolder <- '../../data/Results/Bayesian_linearGLM/adjusted_priors/'
datafolder <- '../../data/Modeling_Data/'
areafolder <- '../../data/Modeling_Data/areas_to_investigate/' # areas that were commonly weird in predictions

feature_type <- 'SGLM'
normalize <- TRUE
random_seed <- 12244
trainrange <- TRUE
novaya_zemlya <- TRUE
greenland <- TRUE
sahara <- TRUE

# Data loading and processing ---------------------------------------------

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
    model<- readRDS(paste(modelfolder, "bayeslinGLM_randCVfeat_model.rds", sep='')) 
  }
  if (feature_type=="SGLM"){
    model <- readRDS(paste(modelfolder, "bayeslinGLM_spatialCVfeat_model.rds", sep='')) 
  }
}

trainfile <- paste(datafolder,'traindata_',feature_type,'.csv',sep='')
pres_testfile <- paste(datafolder,'testdata_pres_',feature_type,'.csv',sep='')
fut_testfile <- paste(datafolder,'testdata_',feature_type,'.csv',sep='')

traindata_master <- read.csv(trainfile, header=TRUE)
pres_testdata <- read.csv(pres_testfile, header=TRUE)
fut_testdata <- read.csv(fut_testfile, header=TRUE)

train_features <- subset(traindata_master, select=-c(HID, Folds, PA))
pres_testfeatures <- subset(pres_testdata, select=-c(HID))
fut_testfeatures <- subset(fut_testdata, select=-c(HID))
test_features <- rbind(pres_testfeatures, fut_testfeatures)

if (novaya_zemlya== TRUE){
  novzem_presfile <- paste(areafolder,'novaya_zemlya_pres',feature_type,'.csv',sep='')
  novzem_futfile <- paste(areafolder,'novaya_zemlya_fut',feature_type,'.csv',sep='')
  novzem_pres <- read.csv(novzem_presfile, header=TRUE)
  novzem_fut <- read.csv(novzem_futfile, header=TRUE)
  novzem_presfeatures <- subset(novzem_pres, select=-c(HID))
  novzem_futfeatures <- subset(novzem_fut, select=-c(HID))
}
if (greenland==TRUE){
  greenland_presfile <- paste(areafolder,'greenland_pres',feature_type,'.csv',sep='')
  greenland_futfile <- paste(areafolder,'greenland_fut',feature_type,'.csv',sep='')
  greenland_pres <- read.csv(greenland_presfile, header=TRUE)
  greenland_fut <- read.csv(greenland_futfile, header=TRUE)
  greenland_presfeatures <- subset(greenland_pres, select=-c(HID))
  greenland_futfeatures <- subset(greenland_fut, select=-c(HID))
}
if (sahara==TRUE){
  sahara_presfile <- paste(areafolder,'sahara_pres',feature_type,'.csv',sep='')
  sahara_futfile <- paste(areafolder,'sahara_fut',feature_type,'.csv',sep='')
  sahara_pres <- read.csv(sahara_presfile, header=TRUE)
  sahara_fut <- read.csv(sahara_futfile, header=TRUE)
  sahara_presfeatures <- subset(sahara_pres, select=-c(HID))
  sahara_futfeatures <- subset(sahara_fut, select=-c(HID))
}

if (normalize==TRUE){
  preProc <- preProcess(train_features, method=c("range"))
  train_features <- predict(preProc, train_features)
  test_features <- predict(preProc, test_features)
  if (novaya_zemlya== TRUE){
    novzem_presfeatures <- predict(preProc, novzem_presfeatures)
    novzem_futfeatures <- predict(preProc, novzem_futfeatures)
  }
  if (greenland==TRUE){
    greenland_presfeatures <- predict(preProc, greenland_presfeatures)
    greenland_futfeatures <- predict(preProc, greenland_futfeatures)
  }
  if (sahara==TRUE){
    sahara_presfeatures <- predict(preProc, sahara_presfeatures)
    sahara_futfeatures <- predict(preProc, sahara_futfeatures)
  }
}
train_features <- cbind(HID=traindata_master$HID, train_features, PA=factor(traindata_master$PA))


# The main process --------------------------------------------------------

# choose feature pair from this list
colnames(train_features)

# specify feature here
feature1 <- "TN90P_IDW1N10"
feature2 <- "WSDI_IDW1N10"

breaks <- c()
colors<- c()
if (trainrange==TRUE){ 
  breaks <- c(breaks, "Train")
  colors <- c(colors, "Train"="#00FF00")}
if (novaya_zemlya==TRUE){
  breaks <- c(breaks, "Present Novaya Zemlya", "Future Novaya Zemlya")
  colors <- c(colors, "Present Novaya Zemlya"="#FF0000", "Future Novaya Zemlya"="#FF3399")
}
if(greenland==TRUE){
  breaks <- c(breaks, "Present Greenland", "Future Greenland")
  colors <- c(colors, "Present Greenland"="#99CCFF", "Future Greenland"="#9933FF")
}
if(sahara==TRUE){
  breaks <- c(breaks, "Present Sahara", "Future Sahara")
  colors <- c(colors, "Present Sahara"="#CC9933", "Future Sahara"="#FF6600")
}




surface_plot <- test_condeff_surface(model, test_features, feature1, feature2)

if(trainrange==TRUE){
  surface_plot <- plot_hollow_rec(surface_plot, train_features, feature1, feature2, col="Train")
}
if(novaya_zemlya==TRUE){
  surface_plot <- plot_hollow_rec(surface_plot, novzem_presfeatures, feature1, feature2, col="Present Novaya Zemlya")
  surface_plot <- plot_hollow_rec(surface_plot, novzem_futfeatures, feature1, feature2, col="Future Novaya Zemlya")
}
if(greenland==TRUE){
  surface_plot <- plot_hollow_rec(surface_plot, greenland_presfeatures, feature1, feature2, col="Present Greenland")
  surface_plot <- plot_hollow_rec(surface_plot, greenland_futfeatures, feature1, feature2, col="Future Greenland")
}
if(sahara==TRUE){
  surface_plot <- plot_hollow_rec(surface_plot, sahara_presfeatures, feature1, feature2, col="Present Sahara")
  surface_plot <- plot_hollow_rec(surface_plot, sahara_futfeatures, feature1, feature2, col="Future Sahara")
}

surface_plot <- surface_plot +  
  scale_color_manual(name='Feature ranges', breaks=breaks,
                                   values=colors) +
  theme(text = element_text(size = 13))         

plot(surface_plot)

