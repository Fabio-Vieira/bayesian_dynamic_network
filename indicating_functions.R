counts <- function(edgelist, riskset, sender = F){
  
  #number of cluster
  n <- length(edgelist)
  
  #number of events
  events <- sapply(edgelist, nrow)
  
  #getting just the sender in each cluster
  if(sender){
    
    actor <- unique(riskset[,c(1,3)])
    names(actor) <- c("sender", "cluster")
    actor <- actor[with(actor, order(cluster, sender)),]
    nact <- tapply(actor[,1], actor[,2], length)
    #Output
    m <- array(0, c(max(nact), n, max(events)))
    
  } else {
    
    #Output
    m <- array(0, c(nrow(riskset), n, max(events)))
    
  }
  
  for(i in 1:n){
    
    for(j in 1:events[i]){
      
      if(sender){
        
        m[which(edgelist[[i]][j,2] == actor[actor[,2]==i,1]),i,j] <- 1
        
      } else {
        
        m[which(edgelist[[i]][j,2] == riskset[riskset[,3]==i,1] &
                  edgelist[[i]][j,3] == riskset[riskset[,3]==i,2]),i,j] <- 1
        
      }
      
    }
    
  }
  
  return(m)
  
}

indicators <- function(edgelist, riskset, sender = F){
  
  #number of cluster
  n <- length(unique(riskset[,3]))
  
  #number of events
  events <- sapply(edgelist, nrow)
  
  #getting actors
  actor <- unique(riskset[,c(1,3)])
  names(actor) <- c("sender", "cluster")
  actor <- actor[with(actor, order(cluster, sender)),]
  
  if(sender){
    
    #output
    ind <- matrix(0, ncol = n, nrow = nrow(actor))
    
  } else {
    
    #output
    ind <- matrix(0, ncol = n, nrow = nrow(riskset))
    
  }
  
  for(i in 1:n){
    
    if(sender){
      
      for(j in 1:events[[i]]){
        
        ind[actor[,2]==i,i] <- 1  
        
      }
      
      
    } else {
      
      ind[riskset[,3] == i,i] <- 1 
      
    }
    
  }
  
  if(!sender){
    
    m <- matrix(0, ncol = n, nrow = max(table(riskset$cluster)))
    
    indexes <- apply(ind, 2, function(x) which(x == 1))
    
      
    for(i in 1:ncol(indexes)){
        
      m[1:length(indexes[,i]),i] <- indexes[,i]
        
    }
    
    return(m)
    
  } else {
    
    m <- matrix(0, ncol = n, nrow = max(table(actor$cluster)))
    
    indexes <- apply(ind, 2, function(x) which(x == 1))
    
    for(i in 1:ncol(indexes)){
      
      m[1:length(indexes[,i]),i] <- indexes[,i]
      
    }
    
    return(m) 
    
  }
  
}
  
intensity <- function(edgelist, riskset){

  #number of cluster
  n <- length(unique(riskset[,3]))
  
  #number of events
  events <- sapply(edgelist, nrow)
  
  #output
  int <- array(0, c(nrow(riskset), n, max(events)))
  
  for(i in 1:n){
    
    for(j in 1:events[[i]]){
      
      int[which(edgelist[[i]][j,2] == riskset[,1]
                & riskset[,3] == i),i,j] <- 1
      
    }
    
  }
  
  #Finally getting the indexes
  actor <- unique(riskset[,c(1,3)])
  names(actor) <- c("sender", "cluster")
  actor <- actor[with(actor, order(cluster, sender)),]
  
  m <- array(0, c(max(table(actor$cluster))-1, n, max(events)))
  
  for(i in 1:n){
    
    for(j in 1:events[i]){
      
      m[1:(length(actor$sender[actor$cluster == i])-1),i,j] <- which(int[,i,j] == 1)
      
    }
    
  }
  
  return(m)
  
} 
