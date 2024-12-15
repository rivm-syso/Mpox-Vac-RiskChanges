##########################################################
#
# MPOX model
#
# 3 sexual activity groups, 
# behavioural adaptations in June&July2022
# vaccination data 25july2022-31dec2023
#
##########################################################
#
# A. Run calculations:
#   -- from start (27apr2022) until endAug2022 
#   -- with vaccination 
#   -- with all generated param.sets
# B. Select param sets with Poisson likelihood
# C. Save: med,CrI of nCases with selectedPar
#          med,CrI of vaccinations
#          model variables (S00,S01,S02, etc) at Tmax
#
##########################################################

#start the clock to measure running time
RunningStart <- Sys.time()

#time sequence
xt   <- seq(0,Tmax,1) 

# number of time units (days) to fit (may be different than Tmax) 
nd <- Tmax 

#---------------------------------------------------------
# LIBRARIES 
#---------------------------------------------------------

library(deSolve) #load R library for solving ODEs
library(ggplot2) #load R library for plotting 
library(reshape2)
library(dplyr)
library(lhs) #load R package for LHS

#---------------------------------------------------------
# INSERT own scripts
#---------------------------------------------------------

source("scripts/modelODEvac.r")       # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
source("scripts/initialState2022.r")  # file with initial state of model variables 
source("scripts/resEpiFunctions.r")   # Rfunction to calculate statistics of columns of a dataframe
source("scripts/selectPoissonLikelihood.r")

#---------------------------------------------------------
# READ FILE WITH VALUES OF UNCERTAIN PARAM
#---------------------------------------------------------

upAll <- read.table(
  file = "param/uncParAll.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

nsim <- nrow(upAll) # number of combinations of uncer.param values = number of model repetitions
Kpar <- ncol(upAll) # number uncer.param
upNames <- colnames(upAll)

#---------------------------------------------------------
# CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day, FOR OUTCOMES YOU WANT TO KEEP
# NOTICE: Each time unit is a new column, starting at t=0; each indiv sim run is a new row
# All other structures have time units as rows! 
#---------------------------------------------------------

nSympMatrix <- matrix(nrow=nsim,ncol=length(xt))
vac1dMatrix <- matrix(nrow=nsim,ncol=length(xt))
vac2dMatrix <- matrix(nrow=nsim,ncol=length(xt))
statesEndAll <- matrix(nrow=nsim,ncol=nv) 
# nv=45 MODEL VARIABLES = (5 states S,E,I,Y,R)x(3 vac.status 0,1,2)x(3 sex.act.groups L,M,H)

#-------------------------
# Loop of nsim runs
#-------------------------

for(i in 1:nsim) {
  tis1    <-   upAll[i,1]
  tis2    <-   upAll[i,2]
  tis3    <-   upAll[i,3]
  beta    <-   upAll[i,4]
  epsilonS<-   upAll[i,5]
  epsilonC<-   upAll[i,6]
  w       <-   upAll[i,7]
  aC3     <-   upAll[i,8]
  T1      <-   upAll[i,9]
  T2      <-   upAll[i,10]
  D1L     <-   upAll[i,11]
  D2L     <-   upAll[i,12]

  D1M <- D1L
  D1H <- D1L
  D2M <- D2L
  D2H <- D2L
  
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
  
  if (i%%200 == 0) { print(i) } #print i, when i-th repetition is completed 
  
  } #end for-loop of model repetitions

print("Loop nsim runs completed")
varNames <- colnames(sol)

#-------------------------
# convert nSymp matrix to data frame (nsim X [Tmax+1])
#-------------------------
nSympAll <- as.data.frame(nSympMatrix)

#---------------------------------------------------------
# cases from model & cases from data (for fitting) 
#---------------------------------------------------------
casesMod  <- nSympAll[,2:(nd+1)] # cases from model
casesData <- gg$ncases[1:nd]     # cases from data

#---------------------------------------------------------
# SELECT param with own function for Poisson likelihood
#---------------------------------------------------------

# function that returns selected param
xx <- selectUP(nd=nd,D=casesData,MM=casesMod,UA=upAll)

#---------------------------------------------------------
# SAVE selected par & repetitions
#---------------------------------------------------------

nSel  <- xx$nSel # number of selected param
irSel <- data.frame(iSel = xx$iSel[1:nSel], rSel = xx$rSel[1:nSel])
upSel <- as.data.frame(xx$US[1:nSel,])
colnames(upSel) <- upNames #names of the columns (ie names of uncPar)

#---------------------------------------------------------
# nCases with selected param WITHOUT repetitions 
#---------------------------------------------------------

# create MWDF data frame with nSelA rows & Tmax columns
nSympSel <- matrix(0,nrow=nSel, ncol=nd)
for (i in 1:nSel) {nSympSel[i,] <- t(casesMod[irSel$iSel[i],])}

#---------------------------------------

RunningEnd <- Sys.time() # STOP the clock
print(RunningEnd - RunningStart) # PRINT RUNNING TIME

################################## END ###################