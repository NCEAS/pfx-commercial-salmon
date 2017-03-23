data {
  int<lower=0> N; # number of time steps
  int<lower=0> M; # number of time series
  matrix[N,M] X; # covariate
  matrix[N,M] Y; # response
}
parameters {
  matrix[M,N] sigma0;
  real sigma1[M];
  real<lower=0,upper=1> ar[M];
  real musigma1;
  #real musigma0;
  real<lower=0> sigsigma1;
  #real<lower=0> sigsigma0;
  real b1[M];
  real mub1;
  real<lower=0> sigb1;
}
transformed parameters {
  matrix[N,M] pred;
  matrix[N,M] pred_sig;
  real effect_sig[M];
  for(m in 1:M) {
    pred[1,m] = 0;
    for(t in 2:N) {
      pred[t,m] = ar[m]*Y[t-1,m] + X[t,m]*b1[m];
      pred_sig[t,m] = exp(sigma0[m,t] + X[t,m]*sigma1[m]);
    }

    effect_sig[m] = log(exp(sigma0[m,1] + 0.33333*sigma1[m] ) / exp(sigma0[m,1]));
  }
}
model {
  musigma1 ~ cauchy(0,5);
  #musigma0 ~ cauchy(0,5);
  mub1 ~ normal(0,3);
  sigsigma1 ~ cauchy(0,5);
  #sigsigma0 ~ cauchy(0,5);
  sigb1 ~ cauchy(0,5);
  for(i in 1:M) {
    sigma0[i,1] ~ normal(0,5); # fixed effect
    sigma1[i] ~ normal(musigma1,sigsigma1); # random slopes
    b1[i] ~ normal(mub1,sigb1); # random slopes
    ar[i] ~ normal(0,1);
    for(t in 2:N) {
      sigma0[i,t] ~ normal(sigma0[i,1], 0.1);
    }

  }


  # likelihood
  for(i in 2:N) {
    for(j in 1:M) {
      Y[i,j] ~ normal(pred[i,j], pred_sig[i,j]);
    }
  }

}
