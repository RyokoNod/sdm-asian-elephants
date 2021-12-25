data{
  int <lower=0> K; //the number of features for each person
  int <lower=0> N_tr; //the number of people in training set
  int <lower=0> N_ts; //the number of people in test set
  
  //the training set
  matrix[N_tr,K] x; 
  int y[N_tr]; //label. abnormal=1, normal=0
  
  //the feature for test set
  matrix[N_ts,K] x_ts; //the features
}

parameters{
  real alpha; //the intercept
  vector[K] beta; //the coefficients
}

model{
  //priors
  alpha ~ normal(0, 100);
  beta ~ normal(0, 10);
  
  y ~ bernoulli_logit(alpha + x*beta);//likelihood
}

generated quantities{
  vector[N_tr] tr_pred;
  vector[N_ts] ts_pred;
  vector[N_tr] tr_probs;
  vector[N_ts] ts_probs;
  
  //output that gives draws of binary predictions for data
  for (i in 1:N_tr){
    tr_pred[i]  = bernoulli_rng(inv_logit(alpha + dot_product(x[i,], beta)));
  }
  for (i in 1:N_ts){
    ts_pred[i]  = bernoulli_rng(inv_logit(alpha + dot_product(x_ts[i,], beta)));
  }
  
  //output that gives draws of the probability of being classified as positive
  for (i in 1:N_tr){
    tr_probs[i]  = inv_logit(alpha + dot_product(x[i,], beta));
  }
  for (i in 1:N_ts){
    ts_probs[i]  = inv_logit(alpha + dot_product(x_ts[i,], beta));
  }
}

