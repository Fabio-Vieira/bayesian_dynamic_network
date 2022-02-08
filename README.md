# Bayesian mixed-effect models for independent dynamic social network data

This repository contains a toy example to run the model described in the paper "Bayesian mixed-effect models for dynamic social network data".

# Paper abstract

Relational event or time-stamped social network data have become increasingly available over the years. Accordingly, statistical methods for such data have also surfaced. These techniques are based on log-linear models of the rates of interactions in a social network via actor covariates and network statistics. Particularly, the use of survival analysis concepts has stimulated the development of powerful
methods over the past decade. These models mainly focus on the analysis of single networks. To date, there are few models that can deal with multiple relational event networks jointly. In this paper, we propose a new Bayesian hierarchical model for multiple relational event sequences. This approach allows inferences at the actor level, which are useful in understanding which effects guide actors’ preferences
in social interactions. We also present Bayes factors for hypothesis testing in this class of models. In addition, a new Bayes factor to test random-effect structures is developed. In this test, we let the prior be determined by the data, alleviating the issue of employing improper priors in Bayes factors and thus preventing the use of ad-hoc choices in absence of prior information. We use data of classroom
interactions among high school students to illustrate the proposed methods.

# Files

# data.rds
Contains a toy data with 10 relational event sequences. This data is stored in a list, with the edgelists, the statistics computed from the sequences (intercept and inertia), the risksets and the true values of the parameters used in the simulations. This files is supposed to serve as an example of how one should have their data in order to pass through the functions that prepare the data to be passed to Stan.

# saving_structure.R
This file saves the list to run the Stan model. 

# run_stan.R
Runs the Stan models.

Folders sender and receiver contain the Stan models described in Section 2 of the paper. The remainder of the files contain functions for data manipulation and that are needed before run the Stan models.
