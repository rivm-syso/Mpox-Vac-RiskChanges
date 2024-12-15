##########################################################
#
# MPOX model
#
##########################################################

#start the clock to measure running time
RunningStart <- Sys.time()

#################################################
# Time parameters 
Tmax <- 670         # max number time units to run
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

#source("scripts/modelODEvac2024A.r")       # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
#source("scripts/initialState2024.r")  # file with initial state of model variables 
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
# Read file with daily numbers of vaccine doses
#---------------------------------------------------------
dataVacNumDaily2024 <- read.table(
  file = "data/dataVacNumDailyMatrix2024.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = TRUE)

#---------------------------------------------------------
# READ FILE with STATES AT TMAX OF PREVIOUS TIME PERIOD
#---------------------------------------------------------

stateEnded <- read.table(  
  file="output/20231231withTransitions/model_state_Sel.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

#---------------------------------------------------------
# Initial state: only susc & recovered (all E=I=Y=0)
#---------------------------------------------------------

N <- c(0.60,0.35,0.05)*250000

E00 <- rep.int(0,ag)
I00 <- c(0,0,0)
Y00 <- rep.int(0,ag)
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

states487Sel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization
states668Sel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization

#---------------------------------------------------------
# Loop of nsim runs. At the end: "sol" has ONLY LAST SIMUL RUN!!!
#---------------------------------------------------------
for(i in 1:nsim) {
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3])
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33])
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 
  
  tis1    <-   upSel[i,1]
  tis2    <-   upSel[i,2]
  tis3    <-   upSel[i,3]
  beta    <-   upSel[i,4]
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

  xpar <- as.list(c(dataVacNumDaily2024=dataVacNumDaily2024,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  for (j in 1:nv) {
    states487Sel[i,j]<- with(c(xpar,sol),{sol[487,(j+1)]})
    states668Sel[i,j]<- with(c(xpar,sol),{sol[668,(j+1)]})  }
  
  if (i%%20 == 0) { print(i) } #print i, when i-th repetition is completed 
  
  } #end for-loop of model repetitions

print("Loop of nsim runs completed")

#---------------------------------------------------------
# SAVE model state (S00, S01, etc) at Tmax for selected param
#---------------------------------------------------------

states487Sel <- as.data.frame(states487Sel)
states668Sel <- as.data.frame(states668Sel)

varNames <- colnames(sol)

colnames(states487Sel) <- varNames[2:(nv+1)]
colnames(states668Sel) <- varNames[2:(nv+1)]

write.table(states487Sel, file=paste(outDir,"model_state_Sel_487.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
write.table(states668Sel, file=paste(outDir,"model_state_Sel_668.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )

#---------------------------------------
# STOP the clock to measure running time
RunningEnd <- Sys.time()
print(RunningEnd - RunningStart)
################################## END ###################