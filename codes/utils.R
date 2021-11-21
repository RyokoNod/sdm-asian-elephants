trainvalsplit <- function(master_data, train_prop, random_seed){
  # <Overview>
  # Given a dataset, this function separates the data into training
  # and validation set. (validation set != test set)
  # <Arguments>
  # master_data: master data with HID, climate features, and PA labels
  # train_prop: what proportion of data needs to be in training set
  # random_seed: the random seed for the split
  # <Returns>
  # List that contains
  # traindata: the shuffled data to use as training set
  # validdata: the shuffled data to use as validation set
  
  set.seed(random_seed) # set random seed
  
  # I am going to get the same proportion of each label,
  # so doing the train/validation split separately
  PA0 <- subset(master_data, PA==0) # hexagons without Elephas presence
  PA1 <- subset(master_data, PA==1) # hexagons with Elephas presence
  
  # get the shuffled indices
  smp_size_PA0 <- floor(train_prop * nrow(PA0))
  smp_size_PA1 <- floor(train_prop * nrow(PA1))
  train_ind_PA0 <- sample(seq_len(nrow(PA0)), size = smp_size_PA0)
  train_ind_PA1 <- sample(seq_len(nrow(PA1)), size = smp_size_PA1)
  
  # select random
  train_PA0 <- PA0[train_ind_PA0, ]
  valid_PA0 <- PA0[-train_ind_PA0, ]
  train_PA1 <- PA1[train_ind_PA1, ]
  valid_PA1 <- PA1[-train_ind_PA1, ]
  
  # create the training and validation data (not shuffled yet)
  traindata <- rbind(train_PA0, train_PA1)
  validdata <- rbind(valid_PA0, valid_PA1)
  
  # shuffle the rows again
  train_ind <- sample(nrow(traindata))
  valid_ind <- sample(nrow(validdata))
  traindata <- traindata[train_ind, ]
  validdata <- validdata[valid_ind, ]
  
  trainval <- list("traindata"=traindata, "validdata"=validdata)
  return(trainval)
}