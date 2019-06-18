data {
  int<lower=1> K;               // num topics
  int<lower=1> M;               // num people
  int<lower=1> Q;               // number of questions
  int<lower=1> N;               // total word instances

  int<lower=1,upper=6> resp[N];  // response
  int<lower=1,upper=Q> que[N];  // question n
  int<lower=1,upper=M> pers[N];  // doc ID for word n

  
  vector<lower=0>[K] alpha;     // topic prior
  vector<lower=0>[6] beta;      // word prior
}
parameters {
  simplex[K] theta;   // topic prevalence
  simplex[6] phi[K,Q];  // word dist for topic k
}

transformed parameters {
  real gamma[M,K];

  for (m in 1:M){
    for (k in 1:K){
      gamma[m,k] = categorical_lpmf(k | theta);
    }
  }

      
  for (n in 1:N){
    for (k in 1:K){
      gamma[pers[n],k] = gamma[pers[n],k] + categorical_lpmf(resp[n] | phi[k,que[n]]);
    }
  }
  
}

model {
  
  theta ~ dirichlet(alpha);
  
  for (k in 1:K){
    for(ii in 1:Q){
      phi[k,ii] ~ dirichlet(beta);
    }
  }



      
  for (m in 1:M)
    target += log_sum_exp(gamma[m]);
          
      // to normalize s.t. gamma[m,k] = log Pr[Z2[m] = k|data]
      // gamma[m] <- gamma[m] - log_sum_exp(gamma[m]);
}

