# Files

# data.rds
Contains a toy data with 10 relational event sequences. This data is stored in a list, with the edgelists, the statistics computed from the sequences (intercept and inertia), the risksets and the true values of the parameters used in the simulations. This files is supposed to serve as an example of how one should have their data in order to pass through the functions that prepare the data to be passed to Stan.

# saving_structure.R
This file saves the list to run the Stan model. 

# run_stan.R
Runs the Stan models.

Folders sender and receiver contain the Stan models described in Section 2 of the paper. The remainder of the files contain functions for data manipulation and that are needed before run the Stan models.
