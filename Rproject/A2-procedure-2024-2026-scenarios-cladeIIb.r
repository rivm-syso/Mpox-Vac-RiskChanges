####################################################################
#
# MPOX MODEL -- Execution procedure 
# Analyses AFTER model fitting & with SELECTED PARAMETER 
#
#---------------------------------------------------- 
# STEPS & SCENARIOS: 
#
# 6. 27apr2022 - 31dec2023: with and without transitions between risk groups
#
# 7. run 2024-2025 with transitions, without vaccinations
# 
# 8.no vac
#   8a. intro  5 cases - file vac0ind05
#   8b. intro 10 cases - file vac0ind10
# 
# 9. create file with hypothet vac in 2024
# 
# 10. 3000 vaccinations start Jul2024
#     10a. intro  5 cases - file vac3ind05
#     10b. intro 10 cases - file vac3ind10
# 
# 11. 3000 vaccinations start oct2024
#     11a. intro  5 cases - file vac3Bind05
#     11b. intro 10 cases - file vac3Bind10
# 
# 14. 30000 vaccinations start Jul2024
#     14a. intro  5 cases - file vac10ind05
#     14b. intro 10 cases - file vac10ind10
# 
# 15. 30000 vaccinations start oct2024
#     15a. intro  5 cases - file vac10Bind05
#     15b. intro 10 cases - file vac10Bind10
#
# 21. OTHER behavioural adaptations 
#     No vac in 2024-2025; Introduce 5 cases on 1may2025,1nov2025
#     a,b: 14d earlier or later
#     c,d: 25% lower/higher reductions
#
# 23. Behavioural adaptations when #cases exceeds 5, 10, or 20
#     No vac in 2024-2025; Introduce 5 cases on 1may2025,1nov2025
#
####################################################################

setwd(dir="Rproject")
source("scripts/model-ode.R")         # file with model equations 

#----------------------------------------------
# STEP 6
#
# Run selected param 27apr2022 till 31dec2023
# with & without transitions between risk groups
#----------------------------------------------
scenario= '6.a.'   
Tmax <- 614 # 31dec2023
outDir <- paste0("output/20231231withTransitions")
stateEndedFile = NULL
source('scripts/run-scenario.R')
source("D2-plots.r")

scenario= '6.b.'
Tmax <- 614 # 31dec2023  
outDir <- paste0("output/20231231withoutTransitions")
stateEndedFile = NULL
source('scripts/run-scenario.R')
source("D2-plots.r")

#----------------------------------------------
# STEP 7
# Run 1-1-2024 to 31-12-2025 (with transitions, without vac) 
#----------------------------------------------
scenario= '7'
Tmax <- 730
outDir <- "output/2024a2025_noInf"
stateEndedFile = "output/20231231withTransitions/model_state_Sel.txt"
source('scripts/run-scenario.R')

#----------------------------------------------
# STEP 8
# No vac in 2024-2025
# Introduce new (index) cases on 1may2025,1nov2025
#----------------------------------------------
vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

Tmax <- 200
outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 8.a. Introduce 5 cases ==>> HOSPITALIZATIONS & DEATHS ADDED
#---------------------------
scenario= '8.a.'
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
write.table(numbYSel, file=paste(outDir,   "nY20250501vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
write.table(numbYSel, file=paste(outDir,   "nY20251101vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 8.b. Introduce 10 cases 
#---------------------------
scenario= '8.b.'
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#----------------------------------------------
# STEP 9: 
# create file with HYPOTHETICAL VACCINATION IN 2024
#----------------------------------------------

source("E2-vac2024-convertVacNumbersToRates.r")

#----------------------------------------------
# STEP 10: 3000vaccinations in Jul-Aug-sep-2024
# Introduce new cases on 1may2025,1nov2025
#----------------------------------------------

# run 2024-2025 without infections, with vac2024
outDir <- "output/2024a2025_noInf_hypothVACA"
scenario= '10'
Tmax <- 670
dataVacNumDaily2024 <- read.table(
  file = "data/dataVacNumDailyMatrix2024.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = TRUE)
stateEndedFile="output/20231231withTransitions/model_state_Sel.txt"
source('scripts/run-scenario.R')
#source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 10.a. Introduce 5 cases 
#---------------------------
scenario= '10.a.'
Tmax <- 200
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf_hypothVACA/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACA/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 10.b. Introduce 10 cases 
#---------------------------
scenario= '10.b.'
Tmax <- 200
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf_hypothVACA/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACA/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#----------------------------------------------
# STEP 11: 3000vaccinations in oct-nov-dec-2024
#----------------------------------------------

scenario= '11'
outDir <- "output/2024a2025_noInf_hypothVACB"
Tmax <- 670
source('scripts/run-scenario.R')
#source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 11.a. Introduce 5 cases 
#---------------------------
scenario= '11.a.'
Tmax <- 200
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf_hypothVACB/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACB/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 11.b. Introduce 10 cases 
#---------------------------
scenario= '11.b.'
Tmax <- 200
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf_hypothVACB/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACB/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#######################################################################
# STEP 14
# 10*3000vaccinations in Jul-Aug-Sep-2024
# Introduce new cases on 1may2025,1nov2025
#######################################################################

scenario= '14'
outDir <- "output/2024a2025_noInf_hypothVACA10"
Tmax <- 670

source('scripts/run-scenario.R')
#source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 14.a Introduce 5 cases 
#---------------------------
scenario= '14.a.'
Tmax <- 200
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf_hypothVACA10/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACA10/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 14.b. Introduce 10 cases 
#---------------------------
scenario= '14.b.'
Tmax <- 200
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf_hypothVACA10/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACA10/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#######################################################################
# STEP 15
# 10*3000vaccinations in Oct-Nov-Dec-2024
# Introduce new cases on 1may2025,1nov2025
#######################################################################

scenario= '15'
outDir <- "output/2024a2025_noInf_hypothVACB10"
Tmax <- 670
source('scripts/run-scenario.R')
#source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 15.a Introduce 5 cases 
#---------------------------
scenario= '15.a.'
Tmax <- 200
Y00 <- c(0,0,5)

stateEndedFile = "output/2024a2025_noInf_hypothVACB10/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACB10/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 15.b. Introduce 10 cases 
#---------------------------
scenario= '15.b.'
Tmax <- 200
Y00 <- c(0,0,10)

stateEndedFile = "output/2024a2025_noInf_hypothVACB10/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf_hypothVACB10/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

######################################################################
#
# 21. OTHER behavioural adaptations 
#     a,b: 14d earlier or later
#     c,d: 25% lower/higher reductions
#
# No vac in 2024-2025; Introduce 5 cases on 1may2025,1nov2025
#
######################################################################

source("scripts/model-ode.R")         # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

Tmax <- 200         

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2025"
I00 <- c(0,0,0)
Y00 <- c(0,0,5)
Fbeta  <- 1
zetaClade <- zeta

# 21.a. 14days later than in 2022
#--------------------
scenario= '21.a.'

Tchange <- 14

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_14dLater.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_14dLater.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )



# 21.b. 14days earlier than in 2022
#--------------------
scenario= '21.b.'

Tchange <- -14

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_14dEarlier.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_14dEarlier.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 21.c. lower reductions
#--------------------
scenario= '21.c.'

r25 <- 0.25

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_lowerRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_lowerRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 21.d. higher reductions
#--------------------
scenario= '21.d.'

r25 <- 1.25
rt25 <- 0.25

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_higherRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_higherRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

######################################################################
#
# 23. Behavioral adaptations when #cases exceeds a level
#
######################################################################

Tmax <- 200         # max number time units to run
vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_changesDependPrev"

I00 <- c(0,0,0)
Y00 <- c(0,0,5)

#------------- when #cases>5 
scenario= '23.a.'
limitValue <- 5

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05prevY5.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05prevY5.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#-------------when #cases>10
scenario= '23.b.'
limitValue <- 10

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05prevY10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05prevY10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#------------- when #cases>20 
scenario= '23.c.'
limitValue <- 20

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_487.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05prevY20.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEndedFile = "output/2024a2025_noInf/model_state_Sel_668.txt"
source('scripts/run-scenario.R')
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05prevY20.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
  
######################################################################
#
# 24. PLOTS & STATS of these scenarios 
#
######################################################################

source("E8-plots.r")
  

##################################### END ############################