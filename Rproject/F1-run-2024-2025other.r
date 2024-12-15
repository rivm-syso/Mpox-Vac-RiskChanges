##########################################################
#
# MPOX model
#
##########################################################
#
# Hypothetical introductions of new mpox cases in the future
# & DIFFERENT behavioural adaptations than in 2022
#
# Calculations start on date of new introductions; run for 200 days
#
##########################################################

#start the clock to measure running time
RunningStart <- Sys.time()

#---------------------------------------------------------
# Time parameters 
Tmax <- 200         # max number time units to run
xt   <- seq(0,Tmax,1) # time sequence 

#---------------------------------------------------------
# LIBRARIES 
#---------------------------------------------------------

library(deSolve) #load R library for solving ODEs
library(ggplot2) #load R library for plotting 
library(reshape2)
library(dplyr)

#---------------------------------------------------------
# INSERT own scripts
#---------------------------------------------------------

# source("scripts/modelODE2024novac.r")       # file with model equations 
# source("scripts/parameters-NewClade.r")        # file with param-values, functions of states, times, etc
# source("scripts/initialState2024.r")  # file with initial state of model variables 
source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc

#---------------------------------------------------------
# READ FILE selected UNCERTAIN PARAM
#---------------------------------------------------------
upSel <- read.table(
  file="param/uncParSel.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

irSel <- read.table(
  file="param/repetSel.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

nSel <- nrow(upSel)      # number of selected paramSets
Kpar  <- ncol(upSel)      # number uncer.param
nsim  <- nSel             # numb of simul.runs
upNames <- colnames(upSel)

#---------------------------------------------------------
# READ FILE with STATES AT TMAX OF PREVIOUS RUN
#---------------------------------------------------------

N <- c(0.60,0.35,0.05)*250000

E00 <- rep.int(0,ag)
#I00 <- c(0,0,3)
#Y00 <- rep.int(0,ag)
E10 <- rep.int(0,ag)
I10 <- rep.int(0,ag)
Y10 <- rep.int(0,ag)
E20 <- rep.int(0,ag)
I20 <- rep.int(0,ag)
Y20 <- rep.int(0,ag)

#---------------------------------------------------------
# CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
#---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS
#---------------------------------------------------------

nSympMatrix  <- matrix(nrow=nsim,ncol=length(xt))
statesEndSel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization

#---------------------------------------------------------
# Loop of nsim runs. 
#---------------------------------------------------------
for(i in 1:nsim) {
  
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3]) - c(vacF1,vacF2,vacF3)
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33]) + c(vacF1,vacF2,vacF3)
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 
  
  tis1    <-   upSel[i,1]
  tis2    <-   upSel[i,2]
  tis3    <-   upSel[i,3]
  beta    <-   upSel[i,4]*Fbeta 
  epsilonS<-   upSel[i,5]
  epsilonC<-   upSel[i,6]
  w       <-   upSel[i,7]
  aC3     <-   upSel[i,8]
  T1      <-   upSel[i,9]
  T2      <-   upSel[i,10]
  D1L     <-   upSel[i,11]
  D2L     <-   upSel[i,12]

  D1M <- D1L
  D1H <- D1L
  D2M <- D2L
  D2H <- D2L
  
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,Fbeta=Fbeta,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD,zetaClade=zetaClade))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days

  if (i%%20 == 0) { print(i) } #print i, when i-th repetition is completed 
  
  } #end for-loop of model repetitions

print("Loop of nsim runs completed")

#---------------------------------------------------------
# Save nSymp as dataFrame (nsim X [Tmax+1]). 
# nSymp[i,j] = #cases with selected par.set i, on time unit j
#---------------------------------------------------------
nSympSel <- as.data.frame(nSympMatrix)

#---------------------------------------
# STOP the clock to measure running time
RunningEnd <- Sys.time()
print(RunningEnd - RunningStart)
################################## END ###################