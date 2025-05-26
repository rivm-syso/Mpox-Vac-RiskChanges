######################################################################
# PLOTS & summary stats
#
# scenarios NEW CLADE
# 1. HIGHER TRANSM PROB
# 2. HIGHER HOSPITAL
# 3. same as in 2022
# Run for scen without vac in 2024-2025, intro 5,10, or 20 cases on 1may/1nov2025
#
######################################################################

source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc

datexA  <- seq(as.Date("2025-05-01"), as.Date("2025-11-17"), by="days")
datexB  <- seq(as.Date("2025-11-01"), as.Date("2026-05-20"), by="days")

outDir <- "output/2025_beh2022_newClade2"
ts <- 100

#-----read files------
d20250501vac0ind05  <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05  <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind05s <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind05_severe.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05s <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind05_severe.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind05t <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind05_transm.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind05t <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind05_transm.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

d20250501vac0ind10  <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind10  <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind10s <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind10_severe.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind10s <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind10_severe.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind10t <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind10_transm.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind10t <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind10_transm.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

d20250501vac0ind20  <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind20.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind20  <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind20.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind20s <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind20_severe.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind20s <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind20_severe.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac0ind20t <- read.table(file = "output/2025_beh2022_newClade2/nSymp20250501vac0ind20_transm.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac0ind20t <- read.table(file = "output/2025_beh2022_newClade2/nSymp20251101vac0ind20_transm.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

#------------------------------------------------------
# Calculate outbreak size first 120d, for each scenario
#------------------------------------------------------

sizes_clade <- data.frame(
  s20250501vac0ind05  = rowSums(as.matrix(d20250501vac0ind05[,1:ts])),
  s20251101vac0ind05  = rowSums(as.matrix(d20251101vac0ind05[,1:ts])),
  s20250501vac0ind05s = rowSums(as.matrix(d20250501vac0ind05s[,1:ts])),
  s20251101vac0ind05s = rowSums(as.matrix(d20251101vac0ind05s[,1:ts])),
  s20250501vac0ind05t = rowSums(as.matrix(d20250501vac0ind05t[,1:ts])),
  s20251101vac0ind05t = rowSums(as.matrix(d20251101vac0ind05t[,1:ts])),
  s20250501vac0ind10  = rowSums(as.matrix(d20250501vac0ind10[,1:ts])),
  s20251101vac0ind10  = rowSums(as.matrix(d20251101vac0ind10[,1:ts])),
  s20250501vac0ind10s = rowSums(as.matrix(d20250501vac0ind10s[,1:ts])),
  s20251101vac0ind10s = rowSums(as.matrix(d20251101vac0ind10s[,1:ts])),
  s20250501vac0ind10t = rowSums(as.matrix(d20250501vac0ind10t[,1:ts])),
  s20251101vac0ind10t = rowSums(as.matrix(d20251101vac0ind10t[,1:ts])),
  s20250501vac0ind20  = rowSums(as.matrix(d20250501vac0ind20[,1:ts])),
  s20251101vac0ind20  = rowSums(as.matrix(d20251101vac0ind20[,1:ts])),
  s20250501vac0ind20s = rowSums(as.matrix(d20250501vac0ind20s[,1:ts])),
  s20251101vac0ind20s = rowSums(as.matrix(d20251101vac0ind20s[,1:ts])),
  s20250501vac0ind20t = rowSums(as.matrix(d20250501vac0ind20t[,1:ts])),
  s20251101vac0ind20t = rowSums(as.matrix(d20251101vac0ind20t[,1:ts]))
)

pd <- 100*(1 - sizes_clade$s20250501vac0ind05s/sizes_clade$s20250501vac0ind05) # %difference between scen"s
c(median(pd),quantile(pd,prob=0.025),quantile(pd,prob=0.075))
pd <- 100*(1 - sizes_clade$s20250501vac0ind05t/sizes_clade$s20250501vac0ind05)
c(median(pd),quantile(pd,prob=0.025),quantile(pd,prob=0.075))

ww <- statsFun(sizes_clade)
write.table(ww, file=paste(outDir,"size_stats_clade.txt",sep="/"),sep="\t",row.names=TRUE,quote=FALSE )

######################################################################
#
# PLOTS WITH CREDIBLE INTERVALS
#
######################################################################

#---------------------------------------------------------
#-------PLOT median & 95%CrI: intro 5 cases in MAY -------
#---------------------------------------------------------

may0 <- statsFun(d20250501vac0ind05)
mays <- statsFun(d20250501vac0ind05s)
mayt <- statsFun(d20250501vac0ind05t)

may2025 <- data.frame(
  datexA,
  datexB,
  may0$epiM,
  may0$epiL,
  may0$epiU,
  mays$epiM,
  mays$epiL,
  mays$epiU,
  mayt$epiM,
  mayt$epiL,
  mayt$epiU    )

# PLOT intro may
png(paste(outDir,"newClade_vac0ind05mayCI_legends.png",sep="/")) 
ggplot(data=may2025)+ 
  geom_line(aes(x=datexA,y=may0$epiM,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=mays$epiM,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=mayt$epiM,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may0$epiL, ymax = may0$epiU), fill = "gray", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = mays$epiL, ymax = mays$epiU), fill = "lightblue", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = mayt$epiL, ymax = mayt$epiU), fill = "pink", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 46)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#---------------------------------------------------------
#-------PLOT median & 95%CrI: intro 5 cases in NOV  -------
#---------------------------------------------------------

nov0 <- statsFun(d20251101vac0ind05)
novs <- statsFun(d20251101vac0ind05s)
novt <- statsFun(d20251101vac0ind05t)

nov2025 <- data.frame(
  datexA,
  datexB,
  nov0$epiM,
  nov0$epiL,
  nov0$epiU,
  novs$epiM,
  novs$epiL,
  novs$epiU,
  novt$epiM,
  novt$epiL,
  novt$epiU    )

png(paste(outDir,"newClade_vac0ind05novCI_legends.png",sep="/")) 
ggplot(data=nov2025)+ 
  geom_line(aes(x=datexA,y=nov0$epiM,  color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=novs$epiM,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=novt$epiM,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov0$epiL, ymax = nov0$epiU), fill = "gray", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = novs$epiL, ymax = novs$epiU), fill = "lightblue", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = novt$epiL, ymax = novt$epiU), fill = "pink", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 46)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#---------------------------------------------------------
#-------PLOT median & 95%CrI: intro 10 cases in MAY -------
#---------------------------------------------------------

may0 <- statsFun(d20250501vac0ind10)
mays <- statsFun(d20250501vac0ind10s)
mayt <- statsFun(d20250501vac0ind10t)

may2025 <- data.frame(
  datexA,
  datexB,
  may0$epiM,
  may0$epiL,
  may0$epiU,
  mays$epiM,
  mays$epiL,
  mays$epiU,
  mayt$epiM,
  mayt$epiL,
  mayt$epiU    )

png(paste(outDir,"newClade_vac0ind10mayCI_legends.png",sep="/")) 
ggplot(data=may2025)+ 
  geom_line(aes(x=datexA,y=may0$epiM,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=mays$epiM,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=mayt$epiM,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may0$epiL, ymax = may0$epiU), fill = "gray", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = mays$epiL, ymax = mays$epiU), fill = "lightblue", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = mayt$epiL, ymax = mayt$epiU), fill = "pink", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 90)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#---------------------------------------------------------
#-------PLOT median & 95%CrI: intro 10 cases in NOV  -------
#---------------------------------------------------------

nov0 <- statsFun(d20251101vac0ind10)
novs <- statsFun(d20251101vac0ind10s)
novt <- statsFun(d20251101vac0ind10t)

nov2025 <- data.frame(
  datexA,
  datexB,
  nov0$epiM,
  nov0$epiL,
  nov0$epiU,
  novs$epiM,
  novs$epiL,
  novs$epiU,
  novt$epiM,
  novt$epiL,
  novt$epiU    )

png(paste(outDir,"newClade_vac0ind10novCI_legends.png",sep="/")) 
ggplot(data=nov2025)+ 
  geom_line(aes(x=datexA,y=nov0$epiM,  color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=novs$epiM,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=novt$epiM,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov0$epiL, ymax = nov0$epiU), fill = "gray", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = novs$epiL, ymax = novs$epiU), fill = "lightblue", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = novt$epiL, ymax = novt$epiU), fill = "pink", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 90)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#---------------------------------------------------------
#-------PLOT median & 95%CrI: intro 20 cases in MAY -------
#---------------------------------------------------------

may0 <- statsFun(d20250501vac0ind20)
mays <- statsFun(d20250501vac0ind20s)
mayt <- statsFun(d20250501vac0ind20t)

may2025 <- data.frame(
  datexA,
  datexB,
  may0$epiM,
  may0$epiL,
  may0$epiU,
  mays$epiM,
  mays$epiL,
  mays$epiU,
  mayt$epiM,
  mayt$epiL,
  mayt$epiU    )

png(paste(outDir,"newClade_vac0ind20mayCI_legends.png",sep="/")) 
ggplot(data=may2025)+ 
  geom_line(aes(x=datexA,y=may0$epiM,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=mays$epiM,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=mayt$epiM,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may0$epiL, ymax = may0$epiU), fill = "gray", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = mays$epiL, ymax = mays$epiU), fill = "lightblue", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = mayt$epiL, ymax = mayt$epiU), fill = "pink", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 160)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#---------------------------------------------------------
#-------PLOT median & 95%CrI: intro 20 cases in NOV  -------
#---------------------------------------------------------

nov0 <- statsFun(d20251101vac0ind20)
novs <- statsFun(d20251101vac0ind20s)
novt <- statsFun(d20251101vac0ind20t)

nov2025 <- data.frame(
  datexA,
  datexB,
  nov0$epiM,
  nov0$epiL,
  nov0$epiU,
  novs$epiM,
  novs$epiL,
  novs$epiU,
  novt$epiM,
  novt$epiL,
  novt$epiU    )

png(paste(outDir,"newClade_vac0ind20novCI_legends.png",sep="/")) 
ggplot(data=nov2025)+ 
  geom_line(aes(x=datexA,y=nov0$epiM,  color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=novs$epiM,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=novt$epiM,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov0$epiL, ymax = nov0$epiU), fill = "gray", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = novs$epiL, ymax = novs$epiU), fill = "lightblue", alpha = 0.5) + # Shaded area
  geom_ribbon(aes(x=datexA,ymin = novt$epiL, ymax = novt$epiU), fill = "pink", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 160)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

######################################################################
#
# PLOTS WITH MEANS ONLY (WITHOUT CREDIBLE INTERVALS)
#
######################################################################

#-----------------------------------------
#-------PLOT means intro 5 cases----------------
#-----------------------------------------
c20250501vac0ind05  <- colMeans(d20250501vac0ind05)
c20251101vac0ind05  <- colMeans(d20251101vac0ind05)
c20250501vac0ind05s <- colMeans(d20250501vac0ind05s)
c20251101vac0ind05s <- colMeans(d20251101vac0ind05s)
c20250501vac0ind05t <- colMeans(d20250501vac0ind05t)
c20251101vac0ind05t <- colMeans(d20251101vac0ind05t)

clade5 <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind05,
  c20251101vac0ind05,
  c20250501vac0ind05s,
  c20251101vac0ind05s,
  c20250501vac0ind05t,
  c20251101vac0ind05t  )

# PLOT intro may & nov
png(paste(outDir,"newClade_vac0ind05_legends.png",sep="/")) 
ggplot(data=clade5)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05s,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05t,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05,   color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05s,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05t,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro may
png(paste(outDir,"newClade_vac0ind05may_legends.png",sep="/")) 
ggplot(data=clade5)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05s,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05t,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 35)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro nov
png(paste(outDir,"newClade_vac0ind05nov_legends.png",sep="/")) 
ggplot(data=clade5)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind05,   color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05s,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05t,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 35)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#-----------------------------------------
#-------PLOT intro 10 cases----------------
#-----------------------------------------
c20250501vac0ind10  <- colMeans(d20250501vac0ind10)
c20251101vac0ind10  <- colMeans(d20251101vac0ind10)
c20250501vac0ind10s <- colMeans(d20250501vac0ind10s)
c20251101vac0ind10s <- colMeans(d20251101vac0ind10s)
c20250501vac0ind10t <- colMeans(d20250501vac0ind10t)
c20251101vac0ind10t <- colMeans(d20251101vac0ind10t)

clade10 <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind10,
  c20251101vac0ind10,
  c20250501vac0ind10s,
  c20251101vac0ind10s,
  c20250501vac0ind10t,
  c20251101vac0ind10t  )

# PLOT intro may & nov
png(paste(outDir,"newClade_vac0ind10_legends.png",sep="/")) 
ggplot(data=clade10)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind10,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind10s,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind10t,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind10,   color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind10s,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind10t,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro may
png(paste(outDir,"newClade_vac0ind10may_legends.png",sep="/")) 
ggplot(data=clade10)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind10,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind10s,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind10t,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 65)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro nov
png(paste(outDir,"newClade_vac0ind10nov_legends.png",sep="/")) 
ggplot(data=clade10)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind10,   color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind10s,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind10t,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 65)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#-----------------------------------------
#-------PLOT intro 20 cases----------------
#-----------------------------------------
c20250501vac0ind20  <- colMeans(d20250501vac0ind20)
c20251101vac0ind20  <- colMeans(d20251101vac0ind20)
c20250501vac0ind20s <- colMeans(d20250501vac0ind20s)
c20251101vac0ind20s <- colMeans(d20251101vac0ind20s)
c20250501vac0ind20t <- colMeans(d20250501vac0ind20t)
c20251101vac0ind20t <- colMeans(d20251101vac0ind20t)

clade20 <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind20,
  c20251101vac0ind20,
  c20250501vac0ind20s,
  c20251101vac0ind20s,
  c20250501vac0ind20t,
  c20251101vac0ind20t  )

# PLOT intro may & nov
png(paste(outDir,"newClade_vac0ind20_legends.png",sep="/")) 
ggplot(data=clade20)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind20,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind20s,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind20t,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind20,   color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind20s,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind20t,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1))  +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro may 
png(paste(outDir,"newClade_vac0ind20may_legends.png",sep="/")) 
ggplot(data=clade20)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind20,   color="As subclade IIb",             linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind20s,  color="Higher hospitalization rate", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind20t,  color="Higher infectivity",          linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 115)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro nov 
png(paste(outDir,"newClade_vac0ind20nov_legends.png",sep="/")) 
ggplot(data=clade20)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind20,   color="As subclade IIb",             linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind20s,  color="Higher hospitalization rate", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind20t,  color="Higher infectivity",          linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 115)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As subclade IIb"     = "black",
    "Higher hospitalization rate" = "deepskyblue",
    "Higher infectivity" = "brown1")) +
  labs(color = 'Subclade') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

################################################ END ########################