##########################################################
#
# MPOX model
#
##########################################################
#
# LHS for UNCERTAIN parameters 
#
# Define ranges & distributions of uncertain parameters
# Run Latin Hypercube Sampling (LHS) 
# to generate a matrix of values for the unc.param
##########################################################

#----------------------------------
# LOAD LIBRARIES & SCRIPTS
#----------------------------------
library(lhs) #load R package for LHS

source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
source("scripts/initialState2022.r")  # file with initial state of model variables 
source("scripts/RandomNumberGenerator.r")   

#----------------------------------
# number of param sets to generate
#----------------------------------

nsim <- 10000 

#----------------------------------
# CALL function to generate param sets
#----------------------------------

upAll <- RNGmyFun(nsim,Kpar,upPriors,upNames)

#----------------------------------
# save table with unc par values
#----------------------------------
write.table(upAll,
  file="param/uncParAll.txt",
  sep="\t",
  row.names=FALSE,
  quote=FALSE)

############### END ######################################