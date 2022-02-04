data {
  //number of clusters
  int N;
  //number of actors in the model
  int actor[N];
  //total size of the riskset, concatenating all clusters
  int J;
  //number of random effects in the model
  int covR;
  //number of events in each cluster
  int events[N];
  //size of the riskset in each cluster
  int riskset[N];
  //data array of random effects
  matrix[sum(riskset),covR] SR[max(events)];
  //array containing the multinomial counts of events
  int counts[sum(riskset),N,max(events)];
  //auxiliary matrix indicating which intensities to sum for the actors
  int actorIntensity[max(actor),N];
  //vector of 1's
  row_vector[N] vecAux;
}

parameters {
  //Parameters for dyadic intensity
  //random effects parameters
  matrix[covR, N] Z;
  //population mean of random effects
  vector[covR] mu;
  //population correlations matrix of population effects
  cholesky_factor_corr[covR] Omega;
  //standard deviation of population effects 
  vector<lower=0>[covR] tau;
  
}


transformed parameters {
  
  //random effects parameters
  matrix[covR, N] beta;

  beta = diag_pre_multiply(tau, Omega) * Z + (mu * vecAux);
  
}

model{
  
  //looping through clusters
  for(i in 1:N){
    
    int ind1[actor[i]];
    
    //declaring dyadic intensity
    vector[actor[i]] lambdaDyad;
    
    ind1 = actorIntensity[1:actor[i],i];
    
    //likelihood part
    //looping through events
    for(j in 1:events[i]){
      
      //computing dyadic intensity
      lambdaDyad = exp(SR[j][ind1,] * beta[,i]);
      
      //fitting the multinomial counts of events
      counts[1:actor[i],i,j] ~ multinomial(lambdaDyad/sum(lambdaDyad));
      
    }
    
  }
  
  //prior distributions dyadic intensity
  Omega ~ lkj_corr_cholesky(2);
  tau ~ cauchy(0, 5);
  mu ~ normal(0, 10);
  
  to_vector(Z) ~ std_normal(); //reparametrization
  
}
