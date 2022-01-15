data{
  int <lower=0> K; //the number of features for each hexagon
  int <lower=0> Ntr; //the number of hexagons in training set
  int <lower=0> Nval; //the number of hexagons in validation set
  
  //the training set
  matrix[Ntr,K] x; //the features
  int y[Ntr]; //label. habitat suitable=1, not suitable=0
  
  //the features for validation set
  matrix[Nval,K] x_val; //the features
}

parameters{
  real intercept; //the intercept
  vector[K] coeffs; //the coefficients
}

model{
  //priors
  intercept ~ normal(0, 10);
  coeffs ~ normal(0, 5);
  
  y ~ bernoulli_logit(intercept + x*coeffs);//likelihood
}

generated quantities{
  vector[Ntr] tr_probs;
  vector[Nval] val_probs;
  
  //output that gives draws of the probability of being classified as positive
  for (i in 1:Ntr){
    tr_probs[i]  = inv_logit(intercept + dot_product(x[i,], coeffs));
  }
  for (i in 1:Nval){
    val_probs[i]  = inv_logit(intercept + dot_product(x_val[i,], coeffs));
  }
}

