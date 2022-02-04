cube_to_cube <- function(statistics, cluster){
  
  #number of events
  events <- sapply(statistics, dim)[3,]
  
  #number of statistics
  stats <- unique(sapply(statistics, dim)[2,])
  
  #number of lines 
  lines <- sum(sapply(statistics, dim)[1,])
  
  #output
  s <- array(NA, dim = c(lines, stats, max(events)))
  
  for(i in 1:length(statistics)){
    
    for(j in 1:events[[i]]){
      
      s[which(cluster == i),,j] <- statistics[[i]][,,j]
      
    }
    
  }
  
  #giving names to the final array
  dimnames(s)[[2]] <- dimnames(statistics[[1]])[[2]]
  
  return(s)
  
}
