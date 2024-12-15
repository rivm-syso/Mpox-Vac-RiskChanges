##########################################################
#
# MPOX model
# Calculate statistics & plots of selected parameters 
#
##########################################################

#---------------------------------------------------------
# LIBRARIES 
#---------------------------------------------------------

library(ggplot2) #load R library for plotting 
library(reshape2)
library(dplyr)

#---------------------------------------------------------
# READ FILE with ncases for selected unc param
#---------------------------------------------------------
nCasesM <- read.table(
  file = paste(outDir,"nSympSel_WithoutRep.txt",sep="/"),
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

#---------------------------------------------------------
# time parameters
#---------------------------------------------------------
Tmax <- ncol(nCasesM)
xt   <- seq(0,Tmax,1) # time sequence 
nd   <- Tmax

#---------------------------------------------------------
# INSERT own scripts
#---------------------------------------------------------

source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
source("scripts/initialState2022.r")  # file with initial state of model variables 
source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc

#---------------------------------------------------------
# READ FILE selected UNCERTAIN PARAM
#---------------------------------------------------------
upSel <- read.table(
  file = "param/uncParSel.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

irSel <- read.table(
  file = "param/repetSel.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

nSel <- nrow(upSel)      # number of selected paramSets
Kpar <- ncol(upSel)      # number uncer.param
nsim <- sum(irSel$rSel) # initial numb of simul.runs
upNames <- colnames(upSel)

#---------------------------------------------------------
# stats,plot,save nCases with repetitions 
#---------------------------------------------------------

MS <- matrix(0,nrow=nsim, ncol=nd)
MS <- nCasesM[rep(1:nSel, times = irSel$rSel), ]

MSDF <- as.data.frame(MS)

#function that returns 3 columns (median,Lower,Upper 95%CrI), rows=timepionts
xt  <- seq(1,Tmax,1)
statsCases <- statsFun(MSDF) 
statsCases <- mutate(statsCases,
  xt = xt,
  datex = as.Date(gg$dateSymp[1:Tmax],"%Y-%m-%d"))

png(paste(outDir,"fig_nSymp_sel.png",sep="/")) 
ggplot(data=statsCases)+ 
  geom_line(aes(x=datex,y=statsCases$epiM), color="red") +
  geom_line(aes(x=datex,y=statsCases$epiL), color="black", linetype="twodash") +
  geom_line(aes(x=datex,y=statsCases$epiU), color="black", linetype="twodash") +
  geom_point(data = dataFM, aes(dateSymp, ncases)) +
  scale_x_date(date_labels = "%d %b %Y",
    breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 months")) +
  theme_light() +
  theme(text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date of symptom onset") +
  ylab("Number mpox cases") 
dev.off()

#------------------------------------------
# % of each sex.activ.group with immunity (vac+infected)
#------------------------------------------

percImm1 <- read.table(file = paste(outDir,"percImm_group1.txt",sep="/"),header = TRUE,sep = "\t",stringsAsFactors = TRUE)
percImm2 <- read.table(file = paste(outDir,"percImm_group2.txt",sep="/"),header = TRUE,sep = "\t",stringsAsFactors = TRUE)
percImm3 <- read.table(file = paste(outDir,"percImm_group3.txt",sep="/"),header = TRUE,sep = "\t",stringsAsFactors = TRUE)

percImm1 <- statsFun(as.data.frame(percImm1[,-Tmax])) 
percImm2 <- statsFun(as.data.frame(percImm2[,-Tmax])) 
percImm3 <- statsFun(as.data.frame(percImm3[,-Tmax])) 

percImm1 <- mutate(percImm1,xt =  xt[2:(Tmax)], datex = as.Date(gg$dateSymp[1:(Tmax-1)],"%Y-%m-%d"))
percImm2 <- mutate(percImm2,xt =  xt[2:(Tmax)], datex = as.Date(gg$dateSymp[1:(Tmax-1)],"%Y-%m-%d"))
percImm3 <- mutate(percImm3,xt =  xt[2:(Tmax)], datex = as.Date(gg$dateSymp[1:(Tmax-1)],"%Y-%m-%d"))

#------------------------------------------
# Number of MSM vaccinated
#------------------------------------------
modVacNum <- read.table(
  file = paste(outDir,"statsVacsSel.txt",sep="/"),
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

modVacNum <- mutate(modVacNum,
  datex = as.Date(dataVacNumDaily$dataDate[2:(Tmax)],"%Y-%m-%d"),
  dataVac1week  = dataVacNumDaily$FirstDoses[2:(Tmax)],
  dataVac2week  = dataVacNumDaily$SecondDoses[2:(Tmax)]
  )

png(paste(outDir,"fig_vac_sel.png",sep="/")) #open pdf file to save the plot
ggplot(data=modVacNum)+ 
  geom_line(aes(x=datex,y=vac1daily), color="red") +
  geom_line(aes(x=datex,y=vac2daily), color="blue") +
  geom_point(aes(x=datex,y=dataVac1week/7),shape=20)+
  geom_point(aes(x=datex,y=dataVac2week/7),shape=20) +
  scale_x_date(date_labels = "%d %b %Y",
    breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 months")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Day of symptom onset") +
  ylab("Number of MSM vaccinated") 
dev.off()

#---------------------------------------------------------
# At Tmax: %infected, %vaccinated, %immune
#---------------------------------------------------------

statesEndSel <- read.table(
  file = paste(outDir,"model_state_Sel.txt",sep="/"),
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

statesEndSel <- within.data.frame(statesEndSel,{
  Vg1 <-     S11+S21
  Vg2 <-     S12+S22
  Vg3 <-     S13+S23
  Rg1 <- R01+R11+R21
  Rg2 <- R02+R12+R22
  Rg3 <- R03+R13+R23
  Imm1 <- Rg1 + Vg1
  Imm2 <- Rg2 + Vg2
  Imm3 <- Rg3 + Vg3
  
  Ng1 <- Imm1+S01+E01+E11+E21+I01+I11+I21+Y01+Y11+Y21
  Ng2 <- Imm2+S02+E02+E12+E22+I02+I12+I22+Y02+Y12+Y22
  Ng3 <- Imm3+S03+E03+E13+E23+I03+I13+I23+Y03+Y13+Y23
  
  perRec1 <- 100*Rg1/Ng1
  perRec2 <- 100*Rg2/Ng2
  perRec3 <- 100*Rg3/Ng3
  perImm1 <- 100*Imm1/Ng1
  perImm2 <- 100*Imm2/Ng2
  perImm3 <- 100*Imm3/Ng3
  perImm23  <- 100*(Imm2+Imm3)/(Ng2+Ng3)
  perImmAll <- 100*(Imm1+Imm2+Imm3)/(Ng1+Ng2+Ng3)
})

statisticsEndSel <- statsFun(statesEndSel)
write.table(statisticsEndSel,
  file=paste(outDir,"statistics_end_Sel.txt",sep="/"),
  sep="\t",
  row.names=TRUE,
  quote=FALSE  )

################################## END ###################