data {
  int<lower=0> N;               //no trials
  int<lower=1> P;               //no fixefs
  int<lower=0> J;               //no subjects
  int<lower=1> n_u;             //no subj ranefs
  
  int<lower=1,upper=J> subj[N]; //subject indicator
  
  row_vector[P] X[N];           //fixef design matrix
  row_vector[n_u] Z_u[N];       //subj ranef design matrix
  
  real<lower=0> rt[N];                 //rt
}

parameters {
  vector[P] beta;               //fixef coefs
  cholesky_factor_corr[n_u] L_u;  //cholesky factor of subj ranef corr matrix
  
  vector<lower=0>[n_u] sigma_u; //subj ranef std
  vector[n_u] z_u[J];           //spherical subj ranef
  
  real<lower=0> sigma_e; //residual std
}

transformed parameters {
  vector[n_u] u[J];             //subj ranefs
  
  {
  matrix[n_u,n_u] Sigma_u;    //subj ranef cov matrix
  Sigma_u <- diag_pre_multiply(sigma_u,L_u);
  for(j in 1:J)
    u[j] <- Sigma_u * z_u[j];
  }
}

model {
  //priors
    beta[1] ~ normal(500,50);
  // The beta priors are implicitly uniform (see paper page 6 )
  L_u ~ lkj_corr_cholesky(2.0);
  for (j in 1:J)
    z_u[j] ~ normal(0,1);
  
  
  //likelihood
  #rt[i] ~ normal(X[i] * beta + Z_u[i] * u[subj[i]],sigma_e);
  for (i in 1:N)
        rt[i] ~ normal(X[i] * beta + Z_u[i] * u[subj[i]],sigma_e);

#    rt[i] ~ normal(X[i] * beta + X_fix[i] * beta_fix + Z_u[i] * u[subj[i]],sigma_e);
}
