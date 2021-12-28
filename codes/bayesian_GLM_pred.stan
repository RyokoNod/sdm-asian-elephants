// This file is made specifically to predict samples for test (future) data
// Do not forget to specify algorithm = "Fixed_param" when sampling sith this model

data{
  int <lower=0> K; // the number of features for each hexagon
  int <lower=0> N_samples; // the number of draws in fitted Stan model
  int <lower=0> N_test; // the number of hexagons in test set
  
  // parameters from fitted Stan model
  vector[N_samples] intercept; // the intercept samples
  matrix[N_samples, K] coeffs; // the coefficient samples
  
  //the features for validation set
  matrix[N_test,K] x_test; //the features
}

parameters{
}

model{
}

generated quantities{
  matrix[N_samples,N_test] test_probs;
  
  //output that gives draws of the probability of elephant presence
  for (nt in 1:N_test){
    for (ns in 1:N_samples){
    test_probs[ns,nt]  = inv_logit(intercept[ns] + dot_product(x_test[nt,], coeffs[ns,]));
    }
  }
}

