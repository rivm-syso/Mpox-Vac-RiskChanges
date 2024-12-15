##########################################################
#
# Combine files of selected param values (into one file)
#
##########################################################

source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc
source("scripts/parameters.r")        # file with param-values, functions of states, times, etc

Tmax <- 125
#--------------------------------
# READ FILES WITH PAR VALUES
#--------------------------------

# READ files with values of unc.param
upSelA <- read.table(file = "param/selABC/uncParSelA.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelB <- read.table(file = "param/selABC/uncParSelB.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelC <- read.table(file = "param/selABC/uncParSelC.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelD <- read.table(file = "param/selABC/uncParSelD.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelE <- read.table(file = "param/selABC/uncParSelE.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelF <- read.table(file = "param/selABC/uncParSelF.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelG <- read.table(file = "param/selABC/uncParSelG.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelH <- read.table(file = "param/selABC/uncParSelH.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelJ <- read.table(file = "param/selABC/uncParSelJ.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelK <- read.table(file = "param/selABC/uncParSelK.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelL <- read.table(file = "param/selABC/uncParSelL.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelM <- read.table(file = "param/selABC/uncParSelM.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelN <- read.table(file = "param/selABC/uncParSelN.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelP <- read.table(file = "param/selABC/uncParSelP.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)
upSelQ <- read.table(file = "param/selABC/uncParSelQ.txt",header = TRUE,sep = "\t", stringsAsFactors = TRUE)

# READ files with numbers of repetitions
irSelA <- read.table(file = "param/selABC/repetSelA.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelB <- read.table(file = "param/selABC/repetSelB.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelC <- read.table(file = "param/selABC/repetSelC.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelD <- read.table(file = "param/selABC/repetSelD.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelE <- read.table(file = "param/selABC/repetSelE.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelF <- read.table(file = "param/selABC/repetSelF.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelG <- read.table(file = "param/selABC/repetSelG.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelH <- read.table(file = "param/selABC/repetSelH.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelJ <- read.table(file = "param/selABC/repetSelJ.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelK <- read.table(file = "param/selABC/repetSelK.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelL <- read.table(file = "param/selABC/repetSelL.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelM <- read.table(file = "param/selABC/repetSelM.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelN <- read.table(file = "param/selABC/repetSelN.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelP <- read.table(file = "param/selABC/repetSelP.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
irSelQ <- read.table(file = "param/selABC/repetSelQ.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)

# number of selected par.sets
nSelVector <- c(nrow(upSelA),nrow(upSelB),nrow(upSelC),nrow(upSelD),nrow(upSelE),
                nrow(upSelF),nrow(upSelG),nrow(upSelH),nrow(upSelJ),nrow(upSelK),
                nrow(upSelL),nrow(upSelM),nrow(upSelN),nrow(upSelP),nrow(upSelQ))
nSel  <- sum(nSelVector)     

#--------------------------------
# COMBINE FILES
#--------------------------------

upSel <- rbind(upSelA,upSelB,upSelC,upSelD,upSelE,upSelF,upSelG,upSelH,upSelJ,upSelK,upSelL,upSelM,upSelN,upSelP,upSelQ)
irSel <- rbind(irSelA,irSelB,irSelC,irSelD,irSelE,irSelF,irSelG,irSelH,irSelJ,irSelK,irSelL,irSelM,irSelN,irSelP,irSelQ)
  
colnames(irSel) <- c("iSel","rSel")

#--------------------------------
# CHECK DIMENSIONS
#--------------------------------
Kpar  <- ncol(upSel)      # number uncer.param
nsim  <- sum(irSel$rSel) # initial numb of simul.runs
print(paste("Kpar = ", Kpar))
print(paste("Selected param.sets: "))
print(nSelVector)
print(paste("Total selected param.sets: ",nSel))

#--------------------------------
# Save combined files
#--------------------------------
write.table(upSel,
  file="param/uncParSel.txt",
  sep="\t",
  row.names=FALSE,
  quote=FALSE  )

write.table(irSel,
  file="param/repetSel.txt",
  sep="\t",
  row.names=FALSE,
  quote=FALSE )

#---------------------------------------------------------
# stats of selected unc par 
#---------------------------------------------------------
# m[rep(1:nrow(m), times = v), ]
# Makes new matrix with the i-th row of matrix m repeated v(i) times from vector v

MSDF <- upSel[rep(1:nSel, times = irSel$rSel),]

upStats <- statsFun(MSDF)
upStats <- cbind(upStats,statsFun(upSel))
colnames(upStats) <- c("upMrep","upLrep","upHrep","upM","upL","upH")

# save Stats dataframe with median & CrI of selected param values (rows = time units)
write.table(upStats,
  file="param/statsUnParSel.txt",
  sep="\t",
  row.names=TRUE,
  quote=FALSE  )

###########################################################
# PLOTS of selected unc par 
###########################################################

png(file="param/uncParam.png") 
par(mfrow=c(4,3)) #plot will be raster of Kpar X Kpar plots
for (i in 1:Kpar)  {
  hist(MSDF[,i], breaks = "Sturges",freq = TRUE,  col = "lightblue",
    xlim = c(upPriors[i,1],upPriors[i,2]), ylim = NULL,
    xlab = upPriors[i,3], ylab = "Frequencies",main = "",
    axes = TRUE, plot = TRUE, labels = FALSE)   }
dev.off() #close plot

png(file="param/uncParamWITHOUTrep.png") 
par(mfrow=c(4,3)) #plot will be raster of Kpar X Kpar plots
for (i in 1:Kpar)  {
  hist(upSel[,i], breaks = "Sturges",freq = TRUE,  col = "lightblue",
    xlim = c(upPriors[i,1],upPriors[i,2]), ylim = NULL,
    xlab = upPriors[i,3], ylab = "Frequencies",main = "",
    axes = TRUE, plot = TRUE, labels = FALSE)   }
dev.off() #close plot

##################################### END #############################