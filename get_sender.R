#This function gets the indexes of the riskset which the sender
#is the observed sender, therefore making the subseting on the
#sender for the multinomial choice in the receiver model

get_sender <- function(edgelist, rs){
  
  actors <- lapply(rs, function(x) sort(unique(c(x[,1],x[,2]))))
  m <- array(0, dim = c(max(sapply(actors, length))-1,
                        length(edgelist),
                        max(sapply(edgelist,nrow))))
  
  for(i in 1:length(edgelist)){
    
    for(j in 1:nrow(edgelist[[i]])){
      
      s <- edgelist[[i]][j,2]
      ind <- which(rs[[i]][,1] == s)
     
      m[1:(length(actors[[i]])-1),i,j] <- ind
       
    }
    
  }
  
  return(m)
  
}
