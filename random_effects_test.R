#This script implements the random effects test accoding to the paper
#This is an example of computing the test for one random effect
#We will need to run a loop around to code to compute the test for all
#random effects in your model

#Here we use the example of the toy data on github, where we had 10 groups

#After saving the samples from Stan (either in receiver_model.rds or sender_model.rds)

#Load posterior samples
post_samples <- readRDS("receiver_model.rds") #this comes from run_stan.R file

library(stringr)

#Extracting the random effects
beta <- post_samples[,str_detect(names(post_samples), "beta")]

#We need to test one random effect at a time

names_rnd <- paste0("beta[", "1", ",", 1:10, "]") #ten is the number of groups, change according to how many groups you have

beta_1 <- beta[,names_rnd]

#Getting posterior estimates (as in the paper)
beta_bar <- colMeans(beta_1)
tau_bar <- apply(beta_1, 2, var)

#Extracting the random-effect prior hyperparameters
sig <- post_samples[,str_detect(names(post_samples), "tau")]^2 #squared because we model the sd's

sig <- sig[,"tau[1]"] #it has to be the variance for the same random effect you're analyzing

sig_bar <- mean(sig)

############################################################
############################################################
############################################################

#Computing the BF

  
#Computing the posterior parameters for the test according to the paper
Mxi <- diff(beta_bar) 
Sxi <- diag(head(tau_bar, -1) + tail(tau_bar, -1))
  
#Compiting variance of the prior according to the paper
PriorXi <- diag(2*(sig_bar), length(beta_bar)-1)
  
rows1 <- 2:length(beta_bar);rows2 <- 1:(length(beta_bar)-1)
cols1 <- 1:(length(beta_bar)-1);cols2 <- 2:length(beta_bar)

#Filling off diagonals according to the paper  
for(j in 1:(ncol(PriorXi) - 1)){
    
  PriorXi[rows1[j],cols1[j]] <- -(sig_bar)
  PriorXi[rows2[j],cols2[j]] <- -(sig_bar)
    
  Sxi[rows1[j],cols1[j]] <- -(tau_bar[rows1[j]])
  Sxi[rows2[j],cols2[j]] <- -(tau_bar[cols2[j]])
    
}
  
prior <- mvtnorm::dmvnorm(rep(0,length(beta_bar)-1), mean = rep(0,length(beta_bar)-1), sigma = PriorXi)
post <- mvtnorm::dmvnorm(rep(0,length(beta_bar)-1), mean = Mxi, sigma = Sxi)
  
(BF <- post/prior)

