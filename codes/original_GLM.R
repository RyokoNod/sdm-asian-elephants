datafolder <- '../data/Modeling_Data/'
datafile <- 'traindata_GLM.csv'

traindata_master <- read.csv(paste(datafolder,datafile,sep=''),header=TRUE)

traindata_PA0 <- subset(traindata_master, PA==0) # hexagons without Elephas presence
traindata_PA1 <- subset(traindata_master, PA==1) # hexagons with Elephas presence

shuffled_PA0