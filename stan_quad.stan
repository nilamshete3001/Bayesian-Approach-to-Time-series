data{
  int n;
  real<lower=0> y[n];
  real<lower=0> time[n];
  real<lower=0> time_sq[n];
  real<lower=0> sea1[n];
  real<lower=0> sea2[n];
  real<lower=0> sea3[n];
  real<lower=0> sea4[n];
}

parameters{
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  real beta6;
  
  real<lower=0, upper=100> sigma;
}

transformed parameters{
  real mu[n];
  for(i in 1:n){
  mu[i] <- beta1*time[i] + beta2*time_sq[i] + beta3*sea1[i] + beta4*sea2[i] + beta5*sea3[i] + beta6*sea4[i];
  }
}

model{
  //Priors
  beta1 ~ normal(0, 7);
  beta2 ~ normal(0, 7);
  beta3 ~ normal(0, 7);
  beta4 ~ normal(0, 7);
  beta5 ~ normal(0, 7);
  beta6 ~ normal(0, 7);
  
  sigma ~ uniform(0, 70);
  
  //Likelihood
  for(i in 1:n){
  y[i] ~ normal(mu[i], sigma);
  }
}