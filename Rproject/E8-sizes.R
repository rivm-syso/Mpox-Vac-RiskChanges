#########################################################
# 
# Calculate OUTBREAK SIZE = TOTAL NUMBER CASES UNTIL t=ts
#
# For all scenarios with subclade IIb
#########################################################

source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc

ts <- 100

############################################################
#
# MAIN SCENARIOS WITH BEHAV ADAPTATIONS AS IN 2022: 
# INTRO 5 OR 10 CASES, 1may2025 or 1nov2025
# vac: 0, 3000, 30000 in aug-oct2024 or feb-apr2025)
#
############################################################

outDir <- "output/2025_beh2022"

d20250501vac0ind05  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind10  <- read.table(file = paste(outDir,"nSymp20250501vac0ind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind10  <- read.table(file = paste(outDir,"nSymp20251101vac0ind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

d20250501vac3ind05  <- read.table(file = paste(outDir,"nSymp20250501vac3ind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3ind05  <- read.table(file = paste(outDir,"nSymp20251101vac3ind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac3ind10  <- read.table(file = paste(outDir,"nSymp20250501vac3ind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3ind10  <- read.table(file = paste(outDir,"nSymp20251101vac3ind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

d20250501vac3Bind05  <- read.table(file = paste(outDir,"nSymp20250501vac3Bind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3Bind05  <- read.table(file = paste(outDir,"nSymp20251101vac3Bind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac3Bind10  <- read.table(file = paste(outDir,"nSymp20250501vac3Bind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3Bind10  <- read.table(file = paste(outDir,"nSymp20251101vac3Bind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

d20250501vac30ind05  <- read.table(file = paste(outDir,"nSymp20250501vac10ind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30ind05  <- read.table(file = paste(outDir,"nSymp20251101vac10ind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac30ind10  <- read.table(file = paste(outDir,"nSymp20250501vac10ind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30ind10  <- read.table(file = paste(outDir,"nSymp20251101vac10ind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

d20250501vac30Bind05  <- read.table(file = paste(outDir,"nSymp20250501vac10Bind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30Bind05  <- read.table(file = paste(outDir,"nSymp20251101vac10Bind05.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac30Bind10  <- read.table(file = paste(outDir,"nSymp20250501vac10Bind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30Bind10  <- read.table(file = paste(outDir,"nSymp20251101vac10Bind10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

sizes1 <- data.frame(
  s20250501vac0ind05  = rowSums(as.matrix(d20250501vac0ind05[,1:ts])),
  s20251101vac0ind05  = rowSums(as.matrix(d20251101vac0ind05[,1:ts])),
  s20250501vac0ind10  = rowSums(as.matrix(d20250501vac0ind10[,1:ts])),
  s20251101vac0ind10  = rowSums(as.matrix(d20251101vac0ind10[,1:ts])),
  s20250501vac3ind05  = rowSums(as.matrix(d20250501vac3ind05[,1:ts])),
  s20251101vac3ind05  = rowSums(as.matrix(d20251101vac3ind05[,1:ts])),
  s20250501vac3ind10  = rowSums(as.matrix(d20250501vac3ind10[,1:ts])),
  s20251101vac3ind10  = rowSums(as.matrix(d20251101vac3ind10[,1:ts])),
  s20250501vac3Bind05  = rowSums(as.matrix(d20250501vac3Bind05[,1:ts])),
  s20251101vac3Bind05  = rowSums(as.matrix(d20251101vac3Bind05[,1:ts])),
  s20250501vac3Bind10  = rowSums(as.matrix(d20250501vac3Bind10[,1:ts])),
  s20251101vac3Bind10  = rowSums(as.matrix(d20251101vac3Bind10[,1:ts])),
  s20250501vac30ind05  = rowSums(as.matrix(d20250501vac30ind05[,1:ts])),
  s20251101vac30ind05  = rowSums(as.matrix(d20251101vac30ind05[,1:ts])),
  s20250501vac30ind10  = rowSums(as.matrix(d20250501vac30ind10[,1:ts])),
  s20251101vac30ind10  = rowSums(as.matrix(d20251101vac30ind10[,1:ts])),
  s20250501vac30Bind05  = rowSums(as.matrix(d20250501vac30Bind05[,1:ts])),
  s20251101vac30Bind05  = rowSums(as.matrix(d20251101vac30Bind05[,1:ts])),
  s20250501vac30Bind10  = rowSums(as.matrix(d20250501vac30Bind10[,1:ts])),
  s20251101vac30Bind10  = rowSums(as.matrix(d20251101vac30Bind10[,1:ts]))
  )

ww <- statsFun(sizes1)
write.table(ww, file=paste(outDir,"size_stats1.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

############################################################
#
# SCENARIOS WITH DIFFERENT BEHAV ADAPTATIONS: 
# 14d earlier/later
# lower/higher reductions
#
############################################################

outDir <- "output/2025_beh2025"

d20250501vac0ind05la  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05_14dLater.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05la  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05_14dLater.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind05ea  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05_14dEarlier.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05ea  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05_14dEarlier.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind05lr  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05_lowerRed.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05lr  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05_lowerRed.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind05hr  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05_higherRed.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05hr  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05_higherRed.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

sizes2 <- data.frame(
  s20250501vac0ind05la  = rowSums(as.matrix(d20250501vac0ind05la[,1:ts])),
  s20251101vac0ind05la  = rowSums(as.matrix(d20251101vac0ind05la[,1:ts])),
  s20250501vac0ind05ea  = rowSums(as.matrix(d20250501vac0ind05ea[,1:ts])),
  s20251101vac0ind05ea  = rowSums(as.matrix(d20251101vac0ind05ea[,1:ts])),
  s20250501vac0ind05lr  = rowSums(as.matrix(d20250501vac0ind05lr[,1:ts])),
  s20251101vac0ind05lr  = rowSums(as.matrix(d20251101vac0ind05lr[,1:ts])),
  s20250501vac0ind05hr  = rowSums(as.matrix(d20250501vac0ind05hr[,1:ts])),
  s20251101vac0ind05hr  = rowSums(as.matrix(d20251101vac0ind05hr[,1:ts]))
)

ww <- statsFun(sizes2)
write.table(ww, file=paste(outDir,"size_stats2.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

############################################################
#
# SCENARIOS WITH DIFFERENT BEHAV ADAPTATIONS: 
# when cases >10 or >20
#
############################################################

outDir <- "output/2025_changesDependPrev"

d20250501vac0ind05y10  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05prevY10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05y10  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05prevY10.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind05y20  <- read.table(file = paste(outDir,"nSymp20250501vac0ind05prevY20.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05y20  <- read.table(file = paste(outDir,"nSymp20251101vac0ind05prevY20.txt",sep="/"),header = TRUE, sep = "\t",stringsAsFactors = TRUE)

sizes3 <- data.frame(
  s20250501vac0ind05y10  = rowSums(as.matrix(d20250501vac0ind05y10[,1:ts])),
  s20251101vac0ind05y10  = rowSums(as.matrix(d20251101vac0ind05y10[,1:ts])),
  s20250501vac0ind05y20  = rowSums(as.matrix(d20250501vac0ind05y20[,1:ts])),
  s20251101vac0ind05y20  = rowSums(as.matrix(d20251101vac0ind05y20[,1:ts]))
)

ww <- statsFun(sizes3)
write.table(ww, file=paste(outDir,"size_stats3.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

###################### END #################################