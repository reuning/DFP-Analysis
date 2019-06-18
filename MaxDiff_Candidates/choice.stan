data {
  int N;
  int K;
  int J; 
  
  int set[N];
  int first[20, J];
  int second[20, J];
  int choice[N];

  int vid[N];
  
  int M;
  int race[M];
  int age[M];
  int gender[M];
  int ideo[M];
  int educ[M];
  int inc[M];


}

parameters {

  vector[K] alpha;
  real<lower=0> race_sigma;
  vector[K] race_raw[3];
  
  real<lower=0> age_sigma;
  vector[K] age_raw[5];
  
  real<lower=0> gender_sigma;
  vector[K] gender_raw[2];
  
  real<lower=0> educ_sigma;
  vector[K] educ_raw[4];
  
  real<lower=0> ideo_sigma;
  vector[K] ideo_raw[5];
  
  real<lower=0> inc_sigma;
  vector[K] inc_raw[6];
}

transformed parameters {
  vector[K] race_beta[3];
  vector[K] age_beta[5];
  vector[K] gender_beta[2];
  vector[K] educ_beta[4];
  vector[K] ideo_beta[5];
  vector[K] inco_beta[6];

  for(ii in 1:2){
    gender_beta[ii] = gender_raw[ii] * gender_sigma;
  }
  
  for(ii in 1:3){
    race_beta[ii] = race_raw[ii] * race_sigma;
  }
  
  for(ii in 1:4){
    educ_beta[ii] = educ_raw[ii] * educ_sigma;
  }
  
  for(ii in 1:5){
    age_beta[ii] = age_raw[ii] * age_sigma;
    ideo_beta[ii] = ideo_raw[ii] * ideo_sigma;

  }
  for(ii in 1:6){
    inco_beta[ii] = inc_raw[ii] * inc_sigma;
  }
  
}

model {
  
  
  alpha[1] ~ normal(0, .1);
  alpha[2:K] ~ normal(0, 1);
  for(ii in 1:2){
    gender_raw[ii] ~ normal(0, 1);
  }
  
  for(ii in 1:3){
    race_raw[ii] ~ normal(0, 1);
  }
  
  for(ii in 1:4){
    educ_raw[ii] ~ normal(0, 1);
  }

  for(ii in 1:5){
    age_raw[ii] ~ normal(0, 1);
    ideo_raw[ii] ~ normal(0, 1);

  }
  
  for(ii in 1:6){
    inc_raw[ii] ~ normal(0, 1);
  }
  
  
  race_sigma ~ normal(0, 1);
  age_sigma ~ normal(0, 1);
  gender_sigma ~ normal(0, 1);
  inc_sigma ~ normal(0, 1);
  ideo_sigma ~ normal(0, 1);
  educ_sigma ~ normal(0, 1);

  
  {
    vector[K] theta;
    
    for(ii in 1:N){
      
      theta = alpha + race_beta[race[vid[ii]]] + age_beta[age[vid[ii]]] 
                    + gender_beta[gender[vid[ii]]] + inco_beta[inc[vid[ii]]] 
                    + ideo_beta[ideo[vid[ii]]] + educ_beta[educ[vid[ii]]] ;

      choice[ii] ~ categorical_logit(theta[first[,set[ii]]] 
                                      - theta[second[,set[ii]]] );
      
    }
  }




  
}

