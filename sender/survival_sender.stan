data {
  //number of clusters
  int N;
  //number of actors in the model
  int actor[N];
  //total size of the riskset, concatenating all clusters
  int J;
  //number of random effects for the actor intensity
  int covRA;
  //number of events in each cluster
  int events[N];
  //size of the riskset in each cluster
  int riskset[N];
  //actors data array of random effexts
  matrix[sum(actor),covRA] SRactor[max(events)];
  //array containing the multinomial counts of actors
  int counts_actor[max(actor),N,max(events)];
  //matrix containing the inter-event times
  real time[max(events),N];
  //auxiliary matrix indicating which intensities to sum for the actors
  int actorIntensity[max(actor),N];
  //vector of 1's
  row_vector[N] vecAux;
}

parameters {

  //parameters for the actor intensities
  //random effects parameters
  matrix[covRA, N] Z_Actor;
  //population mean of random effects
  vector[covRA] muA;
  //population correlations matrix of population effects
  cholesky_factor_corr[covRA] OmegaA;
  //standard deviation of population effects 
  vector<lower=0>[covRA] tauA;
  
}


transformed parameters {
  
  //random effects
  matrix[covRA, N] gamma;

  gamma = diag_pre_multiply(tauA, OmegaA) * Z_Actor + (muA * vecAux);

  
}

model{
  
  //looping through clusters
  for(i in 1:N){
    
    int ind1[actor[i]];
    
    //declaring actor intensity
    vector[actor[i]] lambdaSnd;
    
    ind1 = actorIntensity[1:actor[i],i];
    
    //likelihood part
    //looping through events
    for(j in 1:events[i]){
      
      //computing actor intensity
      lambdaSnd = exp(SRactor[j][ind1,] * gamma[,i]);
      
      //fitting the inter-event times
      time[j,i] ~ exponential(sum(lambdaSnd));
      
      //fitting multinomial counts of actors
      counts_actor[1:actor[i],i,j] ~ multinomial(lambdaSnd/sum(lambdaSnd));
      
    }
    
  }
  
  //prior distributions for actor intensity
  OmegaA ~ lkj_corr_cholesky(2);
  tauA ~ cauchy(0, 5);
  muA ~ normal(0, 10);
  
  to_vector(Z_Actor) ~ std_normal();
  
}
