pointWiseDeviance <- function(gamma, #vector of parameters sender model
                              beta, #vector of parameters receiver model
                              statsSnd, #array of statistics sender
                              statsDyad, #array os stats receiver
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
      
      log_lambdaSnd <- statsSnd[index,,j] %*% gamma[,i]
      log_lambdaRec <- statsDyad[index,,j] %*% beta[,i]
      
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