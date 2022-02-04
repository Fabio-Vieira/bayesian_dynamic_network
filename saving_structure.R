#This is an example run with simulated data, to illustrate the models

#Loading data
data <- readRDS("data.rds")


#separating the data
event_seq <- data$edgelist
riskset <- data$riskset
sender <- data$statistics_snd
dyad <- data$statistics_rec

#Creating the cluster indicator, this will be used in the stan code to indicate
#from which cluster well be sampling from

clusterSnd <- rep(1:length(event_seq), sapply(sender, nrow))
clusterDyad <- rep(1:length(event_seq), sapply(dyad, nrow))

for(i in 1:length(riskset)){riskset[[i]] <- cbind(riskset[[i]], cluster = rep(i, nrow(riskset[[i]])))}

#Joining the risksets

rs <- do.call(rbind, riskset)
names(rs) <- c("sender", "receiver", "cluster")

#We also need to join the statistics
source("cube_to_cube.R")

sSnd <- cube_to_cube(sender, clusterSnd)
sDyad <- cube_to_cube(dyad, clusterDyad)

#Chaging the cubes to lists
sSnd <- lapply(seq(dim(sSnd)[3]), function(x) as.matrix(sSnd[ , , x]))
sDyad <- lapply(seq(dim(sDyad)[3]), function(x) as.matrix(sDyad[ , , x]))

#ONLY RUN THIS PART IF YOUR CLUSTERS DO NOT HAVE THE SAME NUMBER OF EVENTS
#STAN DOES NOT ACCEPT NA's IN THE DATA MATRIX!!!
#for(i in 1:length(sDyad)){
  
  ##Correcting the NA problem
#  sDyad[[i]][which(is.na(sDyad[[i]]))] <- Inf
#  sSnd[[i]][which(is.na(sSnd[[i]]))] <- Inf
  
#}

###################################################################################################
###################################################################################################
###################################################################################################

source("indicating_functions.R")

#indicator of which event happened in the riskset
ctsDyad <- counts(event_seq, rs)

#indicator of which actor sent the event
ctsSnd <- counts(event_seq, rs, T)

#matrix to sum dyadic intensity function
indDyad <- indicators(event_seq, rs)

#matrix to sum sender intensity function
indSnd <- indicators(event_seq, rs, T)

#indicator to sum the intensity on the dyadic part of likelihood
actorInd <- intensity(event_seq, rs)

###################################################################################################
###################################################################################################
###################################################################################################

#Getting inter-event times
source("cbindfill.R")

times <- lapply(event_seq, function(x) diff(c(0, x[,1])))
times <- do.call(cbind.fill, times)

#Run this only if your clusters do not have the same number of events
#times[which(is.na(times))] <- 1e10 Stan does not accept NA's in the data matrix

#Getting number of actors in each cluster
actor <- sapply(tapply(rs$sender, rs$cluster, unique), length)

###################################################################################################
###################################################################################################
###################################################################################################

#Creating list of data to pass on Stan

stanData <- list(N = length(event_seq),
                 actor = actor,
                 J = nrow(rs),
                 covR =  ncol(sDyad[[1]]),
                 covRA =  ncol(sSnd[[1]]),
                 events = sapply(event_seq, nrow),
                 riskset = apply(indDyad, 2, function(x) length(x[x != 0])),
                 SR = sDyad,
                 SRactor = sSnd,
                 counts = ctsDyad,
                 counts_actor = ctsSnd,
                 time = times,
                 indIntensity = indDyad,
                 actorIntensity = indSnd,
                 actorInd = actorInd,
                 vecAux = rep(1, length(event_seq)))


#Correcting the NA problem

saveRDS(stanData, "stan_data.rds")
