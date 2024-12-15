##########################################################
# 
# PARAMETER SELECTION & MODEL FITTING 
#
##########################################################
# FUNCTION THAT SELECTS A SUBSET OF PARAM COMBINATIONS 
# MODEL FITTING TO DATA USING POISSON LIKELIHOOD
#
# nd = number of time points to fit (may be < length(D)
# ns = number of simulations 
# D = vector with data points
# MM = (ns x nd)-matrix with respective model points; rows=simulations, columns=model output per time point
#
# names are: capital letter for vectors; 2-3 capital letters for matrices; small letters for numbers 
##########################################################

selectUP <- function(nd,D,MM,UA){    
  with(as.list(c(nd,D,MM,UA)),{

    ns <- nrow(MM)
    V  <- matrix(nrow=ns,ncol=nd)  
    PL <- matrix(nrow=ns,ncol=1)  
    CPL <- matrix(nrow=ns,ncol=1) 
    CW <- matrix(nrow=ns,ncol=1)  
    CC <- matrix(nrow=ns,ncol=1) 
    U  <- matrix(nrow=ns,ncol=1) 
    rSel <- matrix(nrow=ns,ncol=1) 
    iSel <- matrix(nrow=ns,ncol=1) 
    US <- matrix(nrow=ns,ncol=ncol(UA)) 

    for (j in 1:ns) { 
      V[j,] <- t(((MM[j,]^D) * exp(-MM[j,])) / factorial(D) )# likelihood of each of nd data points for j-th simulation
      PL[j] <- prod(V[j,]) # likelihood of all nd points for j-th simulation 
      CPL[j] <- sum(PL[1:j])
      } # end for (j in 1:ns)

    CW <- CPL/sum(PL) # CW[j] <- sum(PL[1:j])/sum(PL) = cumulative weights
    
    #generate a vector of ns elements from Uniform(0,1)
    U <- runif(ns) 
    
    # count number of elements of U that fall in each interval (0,CW1],(cW1,CW2],...(CW[ns-1],CE[ns]]
    CC[1] <- sum(U <= CW[1])
    for (j in 2:ns) {CC[j] <- sum(U>CW[j-1] & U <= CW[j]) }
    
    #SELECT par.comb with CC[j]>0
    nSel <- 0
    for (j in 1:ns) {
      if (CC[j]>0) {
        nSel <- nSel + 1
        rSel[nSel] <- CC[j] # number of repetitions of nSel-the selected param
        iSel[nSel] <- j     # index of nSel-th selected param in original uncPar file
        US[nSel,] <- t(UA[j,])
      } # end if (CC[j]>0)
      } # end for (j in 1:ns)
    
    print(nSel)
    print(sum(CC>0)) #number of param.comb selected = number of elements CC_j >0
    print(rSel[1:nSel])

    return(list(nSel=nSel,rSel=rSel,iSel=iSel,US=US))
})}

################################### END ##################