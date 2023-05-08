#This script contains a function to compute the point-wise deviance residuals for the mixed-effect extension of the dynamic actor-
#oriented model

pointWiseDeviance <- function(phi, #vector of fixed effects parameters
                              gamma, #vector of parameters sender model
                              psi, #vector of fixed effects parameters
                              beta, #vector of parameters receiver model
                              statsFSnd, #array of fixed effects statistics sender
                              statsRSnd, #array of random effects statistics sender
                              statsFDyad, #array of fixed stats receiver
                              statsRDyad, #array of random stats receiver
                              countsSnd, #array of counts sender
                              countsDyad, #array of counts receiver
                              ind, #matrix of actor indexes
                              events, #number of events per cluster
                              time, #inter-event times
                              actor, #number of actors per cluster
                              N){
  
  ll <- matrix(0, nrow = max(events), ncol = N)
  
  for(i in 1:N){
    
    index <- ind[1:actor[i],i]
    
    for(j in 1:events[i]){
      
      log_lambdaSnd <- (statsFSnd[index,,j] %*% phi) + statsRSnd[index,,j] %*% gamma[,i]
      log_lambdaRec <- (statsFDyad[index,,j] %*% t(psi)) + statsRDyad[index,,j] %*% beta[,i]
      
      ll[j,i] <- -2*((log_lambdaSnd[which(countsSnd[,i,j] == 1)]
                  - time[j,i] * sum(exp(log_lambdaSnd))) + 
        (log_lambdaRec[which(countsDyad[,i,j] == 1)]
                  - log(sum(exp(log_lambdaRec)))))
      
    }
    
  }
  
  pwdeviance <- vector("list", N)
  
  for(i in 1:N){
    
    pwdeviance[[i]] <- ll[1:events[i],i]
    
  }
  
  return(pwdeviance)
  
}
