####################################################################
#
# MPOX MODEL -- Execution procedure 
# Analyses AFTER model fitting & with SELECTED PARAMETER 
#
#---------------------------------------------------- 
# SCENARIOS: 
#
# a. subclade with higher transm prob than IIb
# b. subclade with higher hospitalization rate than IIb
# c. subclade IIb

####################################################################

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

Tmax <- 200         # max number time units to run

outDir <- "output/2025_beh2022_newClade2"
I00 <- c(0,0,0)

scenario = 'A3'

source("scripts/model-ode.R")         # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc


#######################################################################
#
# a.  increased transm prob 
#
#######################################################################

Fbeta  <- 1.1
zetaClade <- zeta
Finf <- 1

# intro 5 cases
#---------------------
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_transm.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_transm.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# intro 10 cases
#---------------------
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind10_transm.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind10_transm.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# intro 20 cases
#---------------------
Y00 <- c(0,0,20)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind20_transm.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind20_transm.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#######################################################################
#
# b.  increased hospital rate
#
#######################################################################

Fbeta  <- 1
zetaClade <- zeta
Finf <- 0.5

# intro 5 cases
#---------------------
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_severe.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_severe.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# intro 10 cases
#---------------------
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind10_severe.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind10_severe.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# intro 20 cases
#---------------------
Y00 <- c(0,0,20)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind20_severe.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind20_severe.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#######################################################################
#
# c. clade IIb (as in 2022)
#
#######################################################################
Fbeta  <- 1
zetaClade <- zeta
Finf <- 1

# intro 5 cases
#---------------------
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# intro 10 cases
#---------------------
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# intro 20 cases
#---------------------
Y00 <- c(0,0,20)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind20.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind20.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

################################################ END ########################