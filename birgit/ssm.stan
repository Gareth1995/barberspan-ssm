// stan version of cwac_ssm.jags (Gareth Edwards)
// July 2024

// https://mc-stan.org/docs/stan-users-guide/missing-data.html

data {
  int<lower=0> Nobs;
  int<lower=0> Nmiss;
  //vector[N] winter;
  array[Nobs] int<lower=1, upper=Nobs + Nmiss> ii_obs;
  array[Nmiss] int<lower=1, upper=Nobs + Nmiss> ii_mis;
  array[Nobs] real summer;
}

transformed data {
  int<lower=0> N = Nobs + Nmiss;
}

parameters {
  real<lower=0> sigma_zeta;
  real<lower=0> sigma_w;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_e;
  real<lower=0> sigma_epsilon;
  
  array[Nmiss] real summer_mis;
  
  array[N] real zeta;
  array[N] real eps;
  array[N] real w;
  real mu_init;
  real beta_init;
  
//  vector[N] mu_wt;
//  vector[N] lambda;
//  array[N] real beta;

  
}

transformed parameters {
  array[N] real y;
  y[ii_obs] = summer;
  y[ii_mis] = summer_mis;

  vector[N] beta;
  vector[N] mu;

  beta[1] = beta_init;
  mu[1] = mu_init;
  
   // lambda[1] ~ normal(0, 1);
  //mu_wt[1] ~ normal(5, 1);
  
  for(i in 2:N){
    //lambda[i] = lambda[i-1] + eps[i]; // winter to summer ratio
    beta[i] = beta[i-1] + zeta[i];
    mu[i] = mu[i-1] + beta[i] + w[i];
    //mu_wt[i] = mu[i] + lambda[i];     // original = mu_wt[w] = mu_t[w] + lambda[w]
  }
  
}

model {
  beta_init ~ normal(0, 1);
  mu_init ~ normal(5, 1);
  
  sigma_zeta ~ exponential(1);
  sigma_w ~ exponential(1);
  sigma_alpha ~ exponential(1);
  sigma_e ~ exponential(1);
  sigma_epsilon ~ exponential(1);
  
  zeta ~ normal(0, sigma_zeta);
  w ~ normal(0, sigma_w);
  eps ~ normal(0, sigma_epsilon);
  

  y ~ normal(mu, sigma_alpha); //Summer counts
  //winter ~ normal(mu_wt, sigma_e); // winter counts

}

