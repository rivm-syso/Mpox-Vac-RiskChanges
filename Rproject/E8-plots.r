##########################################################
#
# MPOX model
# Calculate statistics & make plots of scenarios 2024-2026 
#
##########################################################

Tmax <- 200
xt   <- seq(1,Tmax+1,1) # time sequence 

datexA  <- seq(as.Date("2025-05-01"), as.Date("2025-11-17"), by="days")
datexB  <- seq(as.Date("2025-11-01"), as.Date("2026-05-20"), by="days")

#------------------------------
# LIBRARIES & own scripts
#------------------------------

library(ggplot2) 
library(reshape2)
library(dplyr)

source("scripts/parameters.r")        # file with param-values, functions of states, times, etc
source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc

#------------------------------
#  uncert param
#------------------------------

irSel <- read.table(
  file = "param/repetSel.txt",
  header = TRUE, 
  sep = "\t", 
  stringsAsFactors = TRUE)

nSel <- nrow(irSel)      # number of selected paramSets
#Kpar <- ncol(upSel)      # number uncer.param
nsim <- sum(irSel$rSel) # initial numb of simul.runs
#upNames <- colnames(upSel)

######################################################
# scenarios with 5, 10 new index cases;
# introduced 1may or 1 nov 2025
# without vac; or with 3,000 or 30,000 vaccinations starting jul or oct2024
######################################################

outDir <- "output/2025_beh2022"

c20250501vac0ind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac0ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac0ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

c20250501vac3ind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac3ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac3ind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac3ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac3ind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac3ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac3ind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac3ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

c20250501vac3Bind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac3Bind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac3Bind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac3Bind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac3Bind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac3Bind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac3Bind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac3Bind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

c20250501vac30ind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac10ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac30ind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac10ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac30ind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac10ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac30ind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac10ind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

c20250501vac30Bind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac10Bind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac30Bind05 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac10Bind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac30Bind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac10Bind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac30Bind10 <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac10Bind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

vac1 <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind05,
  c20251101vac0ind05,
  c20250501vac0ind10,
  c20251101vac0ind10,
  c20250501vac3ind05,
  c20251101vac3ind05,
  c20250501vac3ind10,
  c20251101vac3ind10,
  c20250501vac3Bind05,
  c20251101vac3Bind05,
  c20250501vac3Bind10,
  c20251101vac3Bind10,
  c20250501vac30ind05,
  c20251101vac30ind05,
  c20250501vac30ind10,
  c20251101vac30ind10,
  c20250501vac30Bind05,
  c20251101vac30Bind05,
  c20250501vac30Bind10,
  c20251101vac30Bind10)

# 5 new cases - WITH LEGENDS
png(paste(outDir,"vac_0_3_30_ind05_legend.png",sep="/")) 
ggplot(data=vac1)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,  color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3ind05,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3Bind05, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30ind05, color="30,000 vaccinations Aug-Oct 2024", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30Bind05,color="30,000 vaccinations Feb-Apr 2025", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05,  color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3ind05,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3Bind05, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30ind05, color="30,000 vaccinations Aug-Oct 2024", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30Bind05,color="30,000 vaccinations Feb-Apr 2025", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "No vaccinations"     = "black",
    "3,000 vaccinations Aug-Oct 2024" = "red",
    "3,000 vaccinations Feb-Apr 2025" = "pink",
    "30,000 vaccinations Aug-Oct 2024" = "blue",
    "30,000 vaccinations Feb-Apr 2025" = "cyan")) +
  labs(color = 'Vaccination') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# 10 new cases - WITH LEGENDS
png(paste(outDir,"vac_0_3_30_ind10_legend.png",sep="/")) 
ggplot(data=vac1)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind10,  color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3ind10,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3Bind10, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30ind10, color="30,000 vaccinations Aug-Oct 2024", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30Bind10,color="30,000 vaccinations Feb-Apr 2025", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind10,  color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3ind10,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3Bind10, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30ind10, color="30,000 vaccinations Aug-Oct 2024", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30Bind10,color="30,000 vaccinations Feb-Apr 2025", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "No vaccinations"     = "black",
    "3,000 vaccinations Aug-Oct 2024" = "red",
    "3,000 vaccinations Feb-Apr 2025" = "pink",
    "30,000 vaccinations Aug-Oct 2024" = "blue",
    "30,000 vaccinations Feb-Apr 2025" = "cyan")) +
  labs(color = 'Vaccination') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

######################################################
#
# scenarios with different behavioural changes
# -- 14d earlier/later than in 2022
# -- 25% higher/lower reductions than in 2022
#
# Run for scen without vac in 2024-2025, intro 5 cases on 1may/1nov2025
######################################################

c20250501vac0ind05   <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05   <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05la <- colMeans(read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_14dLater.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05la <- colMeans(read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_14dLater.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05ea <- colMeans(read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_14dEarlier.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05ea <- colMeans(read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_14dEarlier.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05lr <- colMeans(read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_lowerRed.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05lr <- colMeans(read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_lowerRed.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05hr <- colMeans(read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_higherRed.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05hr <- colMeans(read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_higherRed.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

beh <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind05,
  c20251101vac0ind05,
  c20250501vac0ind05la,
  c20251101vac0ind05la,
  c20250501vac0ind05ea,
  c20251101vac0ind05ea,
  c20250501vac0ind05lr,
  c20251101vac0ind05lr,
  c20250501vac0ind05hr,
  c20251101vac0ind05hr)

outDir <- "output/2025_beh2025"

#----------------------------------------------------
# 14d earlier/later than in 2022 --- WITH legends
#----------------------------------------------------
png(paste(outDir,"beh_vac0ind05_14d_EarlierLater_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,   color="As in 2022",      linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05la, color="14 days later",   linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05ea, color="14 days earlier", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05,   color="As in 2022",      linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05la, color="14 days later",   linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05ea, color="14 days earlier", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"      = "black",
    "14 days later"   = "green",
    "14 days earlier" = "magenta")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

#----------------------------------------------------------
# lower+higher reductions than in 2022 --- WITH legends
#----------------------------------------------------------
png(paste(outDir,"beh_vac0ind05_lowerHigherRed_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,   color="As in 2022",            linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05lr, color="25% lower reductions",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05hr, color="25% higher reductions", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05,   color="As in 2022",            linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05lr, color="25% lower reductions",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05hr, color="25% higher reductions", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"            = "black",
    "25% lower reductions"  = "green3",
    "25% higher reductions" = "orange")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

######################################################
#
# scenarios different behavioural changes
#  when numb cases >10 or >20
#
# Run for scen without vac in 2024-2025, intro 5 cases on 1may/1nov2025
######################################################

c20250501vac0ind05   <- colMeans(read.table(file = "output/2025_beh2022/nSymp20250501vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05   <- colMeans(read.table(file = "output/2025_beh2022/nSymp20251101vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05y5 <- colMeans(read.table(file = "output/2025_changesDependPrev/nSymp20250501vac0ind05prevY5.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05y5 <- colMeans(read.table(file = "output/2025_changesDependPrev/nSymp20251101vac0ind05prevY5.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05y10 <- colMeans(read.table(file = "output/2025_changesDependPrev/nSymp20250501vac0ind05prevY10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05y10 <- colMeans(read.table(file = "output/2025_changesDependPrev/nSymp20251101vac0ind05prevY10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20250501vac0ind05y20 <- colMeans(read.table(file = "output/2025_changesDependPrev/nSymp20250501vac0ind05prevY20.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05y20 <- colMeans(read.table(file = "output/2025_changesDependPrev/nSymp20251101vac0ind05prevY20.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

beh <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind05,
  c20251101vac0ind05,
  c20250501vac0ind05y5,
  c20251101vac0ind05y5,
  c20250501vac0ind05y10,
  c20251101vac0ind05y10,
  c20250501vac0ind05y20,
  c20251101vac0ind05y20 )

outDir <- "output/2025_changesDependPrev"

#----------------------------------------------------------
# when cases >10 or >20 --- WITH legends
#----------------------------------------------------------
png(paste(outDir,"beh_vac0ind05_y10_y20_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,    color="As in 2022",     linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05y10, color="When >10 cases", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05y20, color="When >20 cases", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05,    color="As in 2022",     linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05y10, color="When >10 cases", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05y20, color="When >20 cases", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"     = "black",
    "When >10 cases" = "lightskyblue1",
    "When >20 cases" = "deepskyblue3")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

################################## END ###################