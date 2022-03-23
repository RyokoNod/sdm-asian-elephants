// generated with brms 2.16.3
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K] b;  // population-level effects
}
transformed parameters {
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += bernoulli_logit_glm_lpmf(Y | X, 0, b);
  }
  // priors including constants
  target += normal_lpdf(b[1] | 0,10);
  target += normal_lpdf(b[2] | 0,5);
  target += normal_lpdf(b[3] | 0,5);
  target += normal_lpdf(b[4] | 0,5);
  target += normal_lpdf(b[5] | 0,5);
  target += normal_lpdf(b[6] | 0,5);
  target += normal_lpdf(b[7] | 0,5);
  target += normal_lpdf(b[8] | 0,5);
  target += normal_lpdf(b[9] | 0,5);
  target += normal_lpdf(b[10] | 0,5);
  target += normal_lpdf(b[11] | 0,5);
}
generated quantities {
}
