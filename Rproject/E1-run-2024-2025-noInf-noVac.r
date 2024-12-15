##########################################################
#
# MPOX model
#
##########################################################
#
# 2024-2025: with transitions between risk groups, without vaccination
#
##########################################################

#start the clock to measure running time
RunningStart <- Sys.time()

#################################################
# Time parameters 
Tmax <- 730           # max number time units to run = 2yrs
xt   <- seq(0,Tmax,1) # time sequence 

outDir <- "output/2024a2025_noInf"

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

source("scripts/modelODE2024novac.r")       # file with model equations 
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
# SIMULATE THE MODEL
#---------------------------------------------------------
# CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
#---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS

pImm1Matrix  <- matrix(nrow=nsim,ncol=length(xt)) # %immune in each risk group
pImm2Matrix  <- matrix(nrow=nsim,ncol=length(xt))
pImm3Matrix  <- matrix(nrow=nsim,ncol=length(xt))

# state variable at time points next analyses will start
# 487=1-5-2025; 668=1-11-2025
states487Sel <- matrix(nrow=nsim,ncol=nv) 
states668Sel <- matrix(nrow=nsim,ncol=nv) 

#---------------------------------------------------------
# Loop of nsim runs
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

  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  pImm1Matrix[i,] <- with(c(xpar,sol),{sol$pImm1}) 
  pImm2Matrix[i,] <- with(c(xpar,sol),{sol$pImm2}) 
  pImm3Matrix[i,] <- with(c(xpar,sol),{sol$pImm3}) 
  for (j in 1:nv) {
    states487Sel[i,j]<- with(c(xpar,sol),{sol[487,(j+1)]}) 
    states668Sel[i,j]<- with(c(xpar,sol),{sol[668,(j+1)]})
      }
  
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

#---------------------------------------------------------
# Save pImm1,pImm2,pImm3 = %immune in sex act group 1,2,3
# save  as dataFrame (nsim X [Tmax+1]). 
# pImm1[i,j] = #cases with selected par.set i, on time unit j
#---------------------------------------------------------

datex25  <- seq(as.Date("2024-01-01"), as.Date("2025-12-30"), by="days")

pImm1 <- as.data.frame(pImm1Matrix)
pImm2 <- as.data.frame(pImm2Matrix)
pImm3 <- as.data.frame(pImm3Matrix)

write.table(pImm1, file=paste(outDir,"percImm_group1.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
write.table(pImm2, file=paste(outDir,"percImm_group2.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
write.table(pImm3, file=paste(outDir,"percImm_group3.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )

percImm1 <- statsFun(as.data.frame(pImm1[,-Tmax])) 
percImm2 <- statsFun(as.data.frame(pImm2[,-Tmax])) 
percImm3 <- statsFun(as.data.frame(pImm3[,-Tmax])) 

percImm1 <- mutate(percImm1,xt =  xt[1:(Tmax)], datex = datex25)
percImm2 <- mutate(percImm2,xt =  xt[1:(Tmax)], datex = datex25)
percImm3 <- mutate(percImm3,xt =  xt[1:(Tmax)], datex = datex25)

percImmStats <- data.frame(
  xt     = percImm1$xt[1:(Tmax)],
  datex25= datex25,
  pImm1M = percImm1$epiM,
  pImm1L = percImm1$epiL,
  pImm1U = percImm1$epiU,
  pImm2M = percImm2$epiM,
  pImm2L = percImm2$epiL,
  pImm2U = percImm2$epiU,
  pImm3M = percImm3$epiM,
  pImm3L = percImm3$epiL,
  pImm3U = percImm3$epiU   )

write.table(percImmStats,
            file=paste(outDir,"percImm_all_stats.txt",sep="/"),
            sep="\t",
            row.names=TRUE,
            quote=FALSE )

#---------------------------------------
# STOP the clock to measure running time
RunningEnd <- Sys.time()
print(RunningEnd - RunningStart)
################################## END ###################