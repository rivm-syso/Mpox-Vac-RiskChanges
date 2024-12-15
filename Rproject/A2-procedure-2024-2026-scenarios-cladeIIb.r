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

# CHANGE DIRECTORY
setwd(dir = "/1-mpox/mpoxNumR")

#----------------------------------------------
# STEP 6
#
# Run selected param 27apr2022 till 31dec2023
# with & without transitions between risk groups
#----------------------------------------------
Tmax <- 614 # 31dec2023 
outDir <- paste0("output/20231231withTransitions")
source("scripts/modelODEvac.r")       # file with model equations 
source("D1-run-selectedParam.r")
source("D2-plots.r")

Tmax <- 614 # 31dec2023 
outDir <- paste0("output/20231231withoutTransitions")
source("scripts/modelODEvac_noTrans.r")       # file with model equations 
source("D1-run-selectedParam.r")
source("D2-plots.r")

#----------------------------------------------
# STEP 7
# Run 1-1-2024 to 31-12-2025 (with transitions, without vac) 
#----------------------------------------------

outDir <- "output/2024a2025_noInf"
source("E1-run-2024-2025-noInf-noVac.r")

#----------------------------------------------
# STEP 8
# No vac in 2024-2025
# Introduce new (index) cases on 1may2025,1nov2025
#----------------------------------------------
vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 8.a. Introduce 5 cases 
#---------------------------
Y00 <- c(0,0,5)

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 8.b. Introduce 10 cases 
#---------------------------
Y00 <- c(0,0,10)

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
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
source("scripts/modelODEvac2024A.r")       
source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 10.a. Introduce 5 cases 
#---------------------------
Y00 <- c(0,0,5)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 10.b. Introduce 10 cases 
#---------------------------
Y00 <- c(0,0,10)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#----------------------------------------------
# STEP 11: 3000vaccinations in oct-nov-dec-2024
#----------------------------------------------

outDir <- "output/2024a2025_noInf_hypothVACB"
source("scripts/modelODEvac2024B.r")       
source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 11.a. Introduce 5 cases 
#---------------------------
Y00 <- c(0,0,5)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 11.b. Introduce 10 cases 
#---------------------------
Y00 <- c(0,0,10)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac3Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac3Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#######################################################################
# STEP 14
# 10*3000vaccinations in Jul-Aug-Sep-2024
# Introduce new cases on 1may2025,1nov2025
#######################################################################

outDir <- "output/2024a2025_noInf_hypothVACA10"
source("scripts/modelODEvac2024A10.r")       
source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 14.a Introduce 5 cases 
#---------------------------
Y00 <- c(0,0,5)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA10/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA10/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10ind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 14.b. Introduce 10 cases 
#---------------------------
Y00 <- c(0,0,10)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA10/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACA10/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10ind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#######################################################################
# STEP 15
# 10*3000vaccinations in Oct-Nov-Dec-2024
# Introduce new cases on 1may2025,1nov2025
#######################################################################

outDir <- "output/2024a2025_noInf_hypothVACB10"
source("scripts/modelODEvac2024B10.r")       
source("E3-run-2024-2025-noInf-vac2024.r")

vacF1 <- 0
vacF2 <- 0
vacF3 <- 0

outDir <- "output/2025_beh2022"
I00 <- c(0,0,0)

# 15.a Introduce 5 cases 
#---------------------------
Y00 <- c(0,0,5)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB10/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB10/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac10Bind05.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 15.b. Introduce 10 cases 
#---------------------------
Y00 <- c(0,0,10)

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB10/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac10Bind10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf_hypothVACB10/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025.r")
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
Tchange <- 14
source("scripts/modelODE2024novac_EarlierLater.r")       # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_14dLater.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_14dLater.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 21.b. 14days earlier than in 2022
#--------------------
Tchange <- -14
source("scripts/modelODE2024novac_EarlierLater.r")       # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_14dEarlier.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_14dEarlier.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 21.c. lower reductions
#--------------------
r25 <- 0.25
source("scripts/modelODE2024novac_lowerRed.r")       # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_lowerRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05_lowerRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

# 21.d. higher reductions
#--------------------
r25 <- 1.25
rt25 <- 0.25
source("scripts/modelODE2024novac_higherRed.r")       # file with model equations 
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05_higherRed.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
source("F1-run-2024-2025other.r")
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
limitValue <- 5
stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
  source("F1-run-2024-2025-JB-changeDependPrev.r")
  write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05prevY5.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
  source("F1-run-2024-2025-JB-changeDependPrev.r")
  write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05prevY5.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#-------------when #cases>10 
limitValue <- 10
stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
  source("F1-run-2024-2025-JB-changeDependPrev.r")
  write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05prevY10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
  source("F1-run-2024-2025-JB-changeDependPrev.r")
  write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05prevY10.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

#------------- when #cases>20 
limitValue <- 20
stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_487.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
  source("F1-run-2024-2025-JB-changeDependPrev.r")
  write.table(nSympSel, file=paste(outDir,"nSymp20250501vac0ind05prevY20.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
stateEnded <- read.table(file="output/2024a2025_noInf/model_state_Sel_668.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
  source("F1-run-2024-2025-JB-changeDependPrev.r")
  write.table(nSympSel, file=paste(outDir,"nSymp20251101vac0ind05prevY20.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )
  
######################################################################
#
# 24. PLOTS & STATS of these scenarios 
#
######################################################################

source("N:/1-mpox/mpoxvac/E8-plots.r")
  

##################################### END ############################