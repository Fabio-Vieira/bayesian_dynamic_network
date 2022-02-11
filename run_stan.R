#This just runs the Stan model
library(rstan);library(shinystan) #loading libraries

data <- readRDS("stan_data.rds")

###################################################################################
###################################################################################
###################################################################################
###################################################################################

#Running the model
t1 <- Sys.time()
receiver_model <- stan_model(paste0(getwd(), "/receiver/multinomial_receiver.stan"))
(t2 <- Sys.time() - t1)

t3 <- Sys.time()
#Updating the model
parallel::detectCores()
options(mc.cores = 1)
fit_receiver <- sampling(receiver_model, data = data, iter = 2000, chains = 1, 
                         init = "0", control = list(adapt_delta = 0.999,
                                                    max_treedepth = 15))
saveRDS(as.data.frame(fit_receiver), "receiver_model.rds") #saving samples
(t4 <- Sys.time() - t3)

###################################################################################
###################################################################################
###################################################################################
###################################################################################

#Running the model
t5 <- Sys.time()
sender_model <- stan_model(paste0(getwd(), "/sender/survival_sender.stan"))
(t6 <- Sys.time() - t5)

t7 <- Sys.time()
#Updating the model
parallel::detectCores()
options(mc.cores = 1)
fit_sender <- sampling(sender_model, data = data, iter = 2000, chains = 1, 
                       init = "0", control = list(adapt_delta = 0.999,
                                                  max_treedepth = 15))
saveRDS(as.data.frame(fit_sender), "sender_model.rds") #saving samples
(t8 <- Sys.time() - t7)
