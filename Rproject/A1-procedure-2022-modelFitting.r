####################################################################
#
# MPOX MODEL
# Execution procedure
#
# 1. Run 10,000 repetitions of the model until Tmax
# 2. Select parameters 
# 3. Repeat steps 1&2 to select large number param.sets
#    Selected param's saved in files A,B,C, etc
# 4. Combine files A,B,C,etc with selected param 
# 
####################################################################

####################################################################

# CALCULATE DAILY VAC NUMBERS IN EACH RISK GROUP FROM TOTAL WEEKLY VAC NUMBERS
source("B-convertVacNumbersToRates.r")

#----------------------------------------------
# STEPs 1-3 repeated a few times (starting with different seed)
#----------------------------------------------

set.seed(1121)
Tmax <- 125     # max number time units to run = 125 days = until 29aug2022
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelA.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file="param/selABC/repetSelA.txt" ,sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1122)  
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelB.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelB.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1123)
Tmax <- 125         
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelC.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelC.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1124)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelD.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelD.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1125)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelE.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelE.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1126)
Tmax <- 125          
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelF.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelF.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1127)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelG.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelG.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1128)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelH.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelH.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1129)
Tmax <- 125         
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelJ.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelJ.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1131)
Tmax <- 125          
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelK.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelK.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1132)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelL.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelL.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1133)
Tmax <- 125          
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelM.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelM.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1134)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelN.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelN.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1135)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelP.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelP.txt",sep="\t",row.names=FALSE,quote=FALSE )

#-----------------------
set.seed(1136)
Tmax <- 125           
source("C1-generateUncPar.r")
source("C2-run-initialPhase-and-selectParam.r")
write.table(upSel,file="param/selABC/uncParSelQ.txt",sep="\t",row.names=FALSE,quote=FALSE )
write.table(irSel,file= "param/selABC/repetSelQ.txt",sep="\t",row.names=FALSE,quote=FALSE )


#----------------------------------------------
# STEP 4
#
# combine files A,B,C,etc into one file with selected param
#----------------------------------------------
source("C3-combine.r")

##################################### END ###############