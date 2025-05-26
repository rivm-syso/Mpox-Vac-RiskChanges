#---------------------------------------------------------
# VARIABLES to be set before running this script
#---------------------------------------------------------
#Tmax <- 200                                                            # max number time units to run
#source("scripts/modelODEvac.r")                                        # file with model equations
#outDir <- "output/2024a2025_noInf"                                     # Output location of results
#scenario= '6.1'                                                        # Scenario id                               
#stateEndedFile = "output/20231231withTransitions/model_state_Sel.txt"  # Something which is constantly different or not there
#source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

#---------------------------------------------------------
# START
#---------------------------------------------------------

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

#TODO: scripts/parameters seems to be consistent over all scenario's, but still it might be interesting to pull it out of this script.
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
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
# READ FILE with STATES AT TMAX OF PREVIOUS TIME PERIOD
#---------------------------------------------------------

if(!is.null(stateEndedFile)){
  stateEnded <- read.table(  
    file=stateEndedFile,
    header = TRUE, 
    sep = "\t", 
    stringsAsFactors = TRUE)
}


#---------------------------------------------------------
# Define some initial state
#---------------------------------------------------------

source("scripts/run-scenario-initial-state.R")  # file with initial state of model variables

#---------------------------------------------------------
# SIMULATE THE MODEL
#---------------------------------------------------------

source('scripts/run-scenario-create-model-variables.R')

#---------------------------------------------------------
# Loop of nsim runs. 
#---------------------------------------------------------
for(i in 1:nsim) {
  
  source('scripts/run-scenario-sim-xstate.R') # Manipulate the xstate for this run
  
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
  
  source('scripts/run-scenario-loop-calcs.R')
  
  if (i%%20 == 0) { print(i) } #print i, when i-th repetition is completed 
  
} #end for-loop of model repetitions

print("Loop of nsim runs completed")

source('scripts/run-scenario-save-results.R')

#---------------------------------------
# STOP the clock to measure running time
RunningEnd <- Sys.time()
print(RunningEnd - RunningStart)
################################## END ###################