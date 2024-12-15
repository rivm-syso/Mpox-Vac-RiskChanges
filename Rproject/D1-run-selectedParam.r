##########################################################
#
# MPOX model
#
##########################################################
#
# Run calculations:
#   -- from start (27apr2022) until 31dec2023 
#   -- with vaccination 
#   -- with selected param.sets
#
##########################################################

#start the clock to measure running time
RunningStart <- Sys.time()

# Time sequence 
xt <- seq(0,Tmax,1)

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

#source("scripts/modelODEvac.r")      # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
source("scripts/initialState2022.r")  # file with initial state of model variables 
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

nSel <- nrow(upSel)       # number of selected paramSets
Kpar  <- ncol(upSel)      # number uncer.param
nsim  <- nSel             # numb of simul.runs
upNames <- colnames(upSel)

#---------------------------------------------------------
# SIMULATE THE MODEL
#---------------------------------------------------------
# CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
#---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS

nSympMatrix  <- matrix(nrow=nsim,ncol=length(xt))
vac1dMatrix  <- matrix(nrow=nsim,ncol=length(xt))
vac2dMatrix  <- matrix(nrow=nsim,ncol=length(xt))
pImm1Matrix  <- matrix(nrow=nsim,ncol=length(xt))
pImm2Matrix  <- matrix(nrow=nsim,ncol=length(xt))
pImm3Matrix  <- matrix(nrow=nsim,ncol=length(xt))
statesEndSel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization

#---------------------------------------------------------
# Loop of nsim runs. 
#---------------------------------------------------------
for(i in 1:nsim) {
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
  
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
  vac1dMatrix[i,] <- with(c(xpar,sol),{sol$vac1}) 
  vac2dMatrix[i,] <- with(c(xpar,sol),{sol$vac2}) 
  pImm1Matrix[i,] <- with(c(xpar,sol),{sol$pImm1}) 
  pImm2Matrix[i,] <- with(c(xpar,sol),{sol$pImm2}) 
  pImm3Matrix[i,] <- with(c(xpar,sol),{sol$pImm3}) 
  for (j in 1:nv) {statesEndSel[i,j]<- with(c(xpar,sol),{sol[Tmax,(j+1)]})}
  
  if (i%%20 == 0) { print(i) } #print i, when i-th repetition is completed 
  
  } #end for-loop of model repetitions

print("Loop of nsim runs completed")

#---------------------------------------------------------
# SAVE model state (S00, S01, etc) at Tmax for selected param
#---------------------------------------------------------

statesEndSel <- as.data.frame(statesEndSel)

varNames <- colnames(sol)
colnames(statesEndSel) <- varNames[2:(nv+1)]

write.table(statesEndSel,  
  file=paste(outDir,"model_state_Sel.txt",sep="/"),
  sep="\t",
  row.names=TRUE,
  quote=FALSE )

#---------------------------------------------------------
# Save nSymp as dataFrame (nsim X [Tmax+1]). 
# nSymp[i,j] = #cases with selected par.set i, on time unit j
#---------------------------------------------------------
nSympSel <- as.data.frame(nSympMatrix)

write.table(nSympSel,  
  file=paste(outDir,"nSympSel_WithoutRep.txt",sep="/"),
  sep="\t",
  row.names=TRUE,
  quote=FALSE )

#---------------------------------------------------------
# Save pImm1,pImm2,pImm3 = %immune in sex act group 1,2,3
# save  as dataFrame (nsim X [Tmax+1]). 
# pImm1[i,j] = #cases with selected par.set i, on time unit j
#---------------------------------------------------------

pImm1 <- as.data.frame(pImm1Matrix)
pImm2 <- as.data.frame(pImm2Matrix)
pImm3 <- as.data.frame(pImm3Matrix)

write.table(pImm1, file=paste(outDir,"percImm_group1.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
write.table(pImm2, file=paste(outDir,"percImm_group2.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
write.table(pImm3, file=paste(outDir,"percImm_group3.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )

#------------------------------------------
# Number of MSM with vaccine protection
#------------------------------------------

vac1d <- as.data.frame(vac1dMatrix[,2:(Tmax+1)]) #extract 1st col for t=0 =26apr
vac2d <- as.data.frame(vac2dMatrix[,2:(Tmax+1)]) #extract 1st col for t=0 =26apr

modVacNum <- data.frame(
  vac1daily    = vac1dMatrix[1,2:(Tmax+1)],
  vac2daily    = vac2dMatrix[1,2:(Tmax+1)],
  vac1dailyCum = cumsum(vac1dMatrix[1,2:(Tmax+1)]),
  vac2dailyCum = cumsum(vac2dMatrix[1,2:(Tmax+1)]))

write.table(modVacNum,
  file=paste(outDir,"statsVacsSel.txt",sep="/"),
  sep="\t",
  row.names=TRUE,
  quote=FALSE )

#---------------------------------------
# STOP the clock to measure running time
RunningEnd <- Sys.time()
print(RunningEnd - RunningStart)
################################## END ###################