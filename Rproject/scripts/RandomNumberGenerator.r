#################################################
# 
# GENERATE MATRIX WITH VALUES FOR UNCERTAIN PARAM
# USING LATIN HYPERCUBE SAMPLING
#
#################################################

RNGmyFun <- function(nsim,Kpar,upPriors,upNames){
  A <- randomLHS(nsim,Kpar) # generate LHS from Uniform(0,1) 
  upAll <- matrix(nrow = nrow(A), ncol = ncol(A)) #create empty matrix upAll
  for (k in 1:Kpar) { 
     upAll[,k] <- qunif(A[,k],min=upPriors[k,1],max=upPriors[k,2]) #Transform to other distributions
     } 
  colnames(upAll) <- upNames #names of columns = names of uncPar)
  return(upAll)
}

###############################END###############