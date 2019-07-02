data {
  int N; //Number of surveyors
  int J; //number of unique elected


  int r_N; //number of elected scores
  int Y[r_N]; //elected scores
  int vid[r_N]; //elected score id
  int rid[r_N]; //ID of the elected (for cases when multiple papers are scored)

  // vector[J+7] theta_mu;
}




parameters {
  ordered[max(Y)-1] cut_mu;
  // ordered[4] cut_ind[N];
  // real<lower=0> cut_sd;
  vector[N] beta;

  // real alpha_mu;
  // real<lower=0> alpha_sd;
  vector[N] alpha;

  //vector<lower=0>[N] tau_ind;
  //vector<lower=0>[8] tau_paper;

  vector[J] theta;
  //real<lower=0> v;
  //real<lower=0> w;

}

model {

  // alpha_mu ~ normal(0, 5);
  // alpha_sd ~ gamma(1, 5);
  // alpha ~ normal(alpha_mu, alpha_sd);
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);

  for(ii in 2:4){
    cut_mu[ii] - cut_mu[ii-1] ~ gamma(1, 1);
  }

  theta[1] ~ normal(-1, .01);
  theta[2] ~ normal(1, .01);
  theta[3:J] ~ normal(0, 1);


  Y ~ ordered_logistic(beta[vid] .* theta[rid] + alpha[vid], cut_mu);


}
