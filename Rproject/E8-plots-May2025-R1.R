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
nsim <- sum(irSel$rSel) # initial numb of simul.runs

######################################################
# 
# HOSPITALIZATIONS 
#
# Scenarios: 5 new cases in may&nov, without vaccinations
#
######################################################

c20250501vac0ind05 <- colMeans(read.table(file = "output/2025_beh2022/nY20250501vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))
c20251101vac0ind05 <- colMeans(read.table(file = "output/2025_beh2022/nY20251101vac0ind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE))

hosp <- data.frame(
  datexA,
  datexB,
  c20250501vac0ind05,
  c20251101vac0ind05)

png(paste(outDir,"hospit_deaths_may.png",sep="/")) 
ggplot(data=hosp)+ 
  geom_line(aes(x=datexA,y=zeta*c20250501vac0ind05), color="blue") +
  geom_line(aes(x=datexA,y= muD*c20250501vac0ind05), color="red"  ) +
  scale_x_date(date_labels = "%d %b %Y",breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  theme_light() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily numbers") 
dev.off()

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

#------------------------------------------ 
# 5 new cases - WITH LEGENDS
#------------------------------------------ 
# Intro 1may2025
png(paste(outDir,"vac_0_3_30_ind05_may_legend.png",sep="/")) 
ggplot(data=vac1)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,  color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3ind05,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3Bind05, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30ind05, color="30,000 vaccinations Aug-Oct 2024", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30Bind05,color="30,000 vaccinations Feb-Apr 2025", linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 20)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# Intro 1nov2025
png(paste(outDir,"vac_0_3_30_ind05_nov_legend.png",sep="/")) 
ggplot(data=vac1)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind05,  color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3ind05,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3Bind05, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30ind05, color="30,000 vaccinations Aug-Oct 2024", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30Bind05,color="30,000 vaccinations Feb-Apr 2025", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 20)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

#------------------------------------------ 
# 10 new cases - WITH LEGENDS
#------------------------------------------ 
# Intro 1may2025
png(paste(outDir,"vac_0_3_30_ind10_may_legend.png",sep="/")) 
ggplot(data=vac1)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind10,  color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3ind10,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac3Bind10, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30ind10, color="30,000 vaccinations Aug-Oct 2024", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac30Bind10,color="30,000 vaccinations Feb-Apr 2025", linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 40)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# Intro 1nov2025
png(paste(outDir,"vac_0_3_30_ind10_nov_legend.png",sep="/")) 
ggplot(data=vac1)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind10,  color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3ind10,  color="3,000 vaccinations Aug-Oct 2024",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac3Bind10, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30ind10, color="30,000 vaccinations Aug-Oct 2024", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac30Bind10,color="30,000 vaccinations Feb-Apr 2025", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 40)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# PLOTS WITH CREDIBLE INTERVALS 
# INTRO 5 CASES in MAY
#
######################################################

d20250501vac0ind05   <- read.table(file = "output/2025_beh2022/nSymp20250501vac0ind05.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac3ind05   <- read.table(file = "output/2025_beh2022/nSymp20250501vac3ind05.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac3Bind05  <- read.table(file = "output/2025_beh2022/nSymp20250501vac3Bind05.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac30ind05  <- read.table(file = "output/2025_beh2022/nSymp20250501vac10ind05.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac30Bind05 <- read.table(file = "output/2025_beh2022/nSymp20250501vac10Bind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

may000 <- statsFun(d20250501vac0ind05)
may030 <- statsFun(d20250501vac3ind05)
may03B <- statsFun(d20250501vac3Bind05)
may300 <- statsFun(d20250501vac30ind05)
may30B <- statsFun(d20250501vac30Bind05)

may <- data.frame(
  datexA,
  datexB,
  may000$epiM,
  may000$epiL,
  may000$epiU,
  may030$epiM,
  may030$epiL,
  may030$epiU,
  may03B$epiM,
  may03B$epiL,
  may03B$epiU,
  may300$epiM,
  may300$epiL,
  may300$epiU,
  may30B$epiM,
  may30B$epiL,
  may30B$epiU   )

# PLOT intro may & vac in Aug
png(paste(outDir,"vac_ind05mayCIa_legends.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=may000$epiM, color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may030$epiM, color="3,000 vaccinations Aug-Oct 2024",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may300$epiM, color="30,000 vaccinations Aug-Oct 2024", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may000$epiL, ymax = may000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may030$epiL, ymax = may030$epiU), fill = "brown1", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may300$epiL, ymax = may300$epiU), fill = "lightblue", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 30)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# PLOT intro may & vac in Feb
png(paste(outDir,"vac_ind05mayCIb_legends.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=may000$epiM, color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may03B$epiM, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may30B$epiM, color="30,000 vaccinations Feb-Apr 2025", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may000$epiL, ymax = may000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may03B$epiL, ymax = may03B$epiU), fill = "pink", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may30B$epiL, ymax = may30B$epiU), fill = "lightblue1", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 30)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# PLOTS WITH CREDIBLE INTERVALS 
# INTRO 5 CASES in NOV
#
######################################################

d20251101vac0ind05   <- read.table(file = "output/2025_beh2022/nSymp20251101vac0ind05.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3ind05   <- read.table(file = "output/2025_beh2022/nSymp20251101vac3ind05.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3Bind05  <- read.table(file = "output/2025_beh2022/nSymp20251101vac3Bind05.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30ind05  <- read.table(file = "output/2025_beh2022/nSymp20251101vac10ind05.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30Bind05 <- read.table(file = "output/2025_beh2022/nSymp20251101vac10Bind05.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

nov000 <- statsFun(d20251101vac0ind05)
nov030 <- statsFun(d20251101vac3ind05)
nov03B <- statsFun(d20251101vac3Bind05)
nov300 <- statsFun(d20251101vac30ind05)
nov30B <- statsFun(d20251101vac30Bind05)

nov <- data.frame(
  datexA,
  datexB,
  nov000$epiM,
  nov000$epiL,
  nov000$epiU,
  nov030$epiM,
  nov030$epiL,
  nov030$epiU,
  nov03B$epiM,
  nov03B$epiL,
  nov03B$epiU,
  nov300$epiM,
  nov300$epiL,
  nov300$epiU,
  nov30B$epiM,
  nov30B$epiL,
  nov30B$epiU   )

# PLOT intro nov & vac in Aug
png(paste(outDir,"vac_ind05novCIa_legends.png",sep="/")) 
ggplot(data=nov)+ 
  geom_line(aes(x=datexA,y=nov000$epiM, color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov030$epiM, color="3,000 vaccinations Aug-Oct 2024",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov300$epiM, color="30,000 vaccinations Aug-Oct 2024", linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov000$epiL, ymax = nov000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov030$epiL, ymax = nov030$epiU), fill = "brown1", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov300$epiL, ymax = nov300$epiU), fill = "lightblue", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 30)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# PLOT intro nov & vac in Feb
png(paste(outDir,"vac_ind05novCIb_legends.png",sep="/")) 
ggplot(data=nov)+ 
  geom_line(aes(x=datexA,y=nov000$epiM, color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov03B$epiM, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov30B$epiM, color="30,000 vaccinations Feb-Apr 2025", linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov000$epiL, ymax = nov000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov03B$epiL, ymax = nov03B$epiU), fill = "pink", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov30B$epiL, ymax = nov30B$epiU), fill = "lightblue1", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 30)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# PLOTS WITH CREDIBLE INTERVALS 
# INTRO 10 CASES in MAY
#
######################################################

d20250501vac0ind10   <- read.table(file = "output/2025_beh2022/nSymp20250501vac0ind10.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac3ind10   <- read.table(file = "output/2025_beh2022/nSymp20250501vac3ind10.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac3Bind10  <- read.table(file = "output/2025_beh2022/nSymp20250501vac3Bind10.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac30ind10  <- read.table(file = "output/2025_beh2022/nSymp20250501vac10ind10.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20250501vac30Bind10 <- read.table(file = "output/2025_beh2022/nSymp20250501vac10Bind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

may000 <- statsFun(d20250501vac0ind10)
may030 <- statsFun(d20250501vac3ind10)
may03B <- statsFun(d20250501vac3Bind10)
may300 <- statsFun(d20250501vac30ind10)
may30B <- statsFun(d20250501vac30Bind10)

may <- data.frame(
  datexA,
  datexB,
  may000$epiM,
  may000$epiL,
  may000$epiU,
  may030$epiM,
  may030$epiL,
  may030$epiU,
  may03B$epiM,
  may03B$epiL,
  may03B$epiU,
  may300$epiM,
  may300$epiL,
  may300$epiU,
  may30B$epiM,
  may30B$epiL,
  may30B$epiU   )

# PLOT intro may & vac in Aug
png(paste(outDir,"vac_ind10mayCIa_legends.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=may000$epiM, color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may030$epiM, color="3,000 vaccinations Aug-Oct 2024",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may300$epiM, color="30,000 vaccinations Aug-Oct 2024", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may000$epiL, ymax = may000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may030$epiL, ymax = may030$epiU), fill = "brown1", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may300$epiL, ymax = may300$epiU), fill = "lightblue", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 55)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# PLOT intro may & vac in Feb
png(paste(outDir,"vac_ind10mayCIb_legends.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=may000$epiM, color="No vaccinations",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may03B$epiM, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=may30B$epiM, color="30,000 vaccinations Feb-Apr 2025", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = may000$epiL, ymax = may000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may03B$epiL, ymax = may03B$epiU), fill = "pink", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = may30B$epiL, ymax = may30B$epiU), fill = "lightblue1", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 55)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# PLOTS WITH CREDIBLE INTERVALS 
# INTRO 10 CASES in NOV
#
######################################################

d20251101vac0ind10   <- read.table(file = "output/2025_beh2022/nSymp20251101vac0ind10.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3ind10   <- read.table(file = "output/2025_beh2022/nSymp20251101vac3ind10.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac3Bind10  <- read.table(file = "output/2025_beh2022/nSymp20251101vac3Bind10.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30ind10  <- read.table(file = "output/2025_beh2022/nSymp20251101vac10ind10.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d20251101vac30Bind10 <- read.table(file = "output/2025_beh2022/nSymp20251101vac10Bind10.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

nov000 <- statsFun(d20251101vac0ind10)
nov030 <- statsFun(d20251101vac3ind10)
nov03B <- statsFun(d20251101vac3Bind10)
nov300 <- statsFun(d20251101vac30ind10)
nov30B <- statsFun(d20251101vac30Bind10)

nov <- data.frame(
  datexA,
  datexB,
  nov000$epiM,
  nov000$epiL,
  nov000$epiU,
  nov030$epiM,
  nov030$epiL,
  nov030$epiU,
  nov03B$epiM,
  nov03B$epiL,
  nov03B$epiU,
  nov300$epiM,
  nov300$epiL,
  nov300$epiU,
  nov30B$epiM,
  nov30B$epiL,
  nov30B$epiU   )

# PLOT intro nov & vac in Aug
png(paste(outDir,"vac_ind10novCIa_legends.png",sep="/")) 
ggplot(data=nov)+ 
  geom_line(aes(x=datexA,y=nov000$epiM, color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov030$epiM, color="3,000 vaccinations Aug-Oct 2024",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov300$epiM, color="30,000 vaccinations Aug-Oct 2024", linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov000$epiL, ymax = nov000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov030$epiL, ymax = nov030$epiU), fill = "brown1", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov300$epiL, ymax = nov300$epiU), fill = "lightblue", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 55)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# PLOT intro nov & vac in Feb
png(paste(outDir,"vac_ind10novCIb_legends.png",sep="/")) 
ggplot(data=nov)+ 
  geom_line(aes(x=datexA,y=nov000$epiM, color="No vaccinations",                  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov03B$epiM, color="3,000 vaccinations Feb-Apr 2025",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexA,y=nov30B$epiM, color="30,000 vaccinations Feb-Apr 2025", linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = nov000$epiL, ymax = nov000$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov03B$epiL, ymax = nov03B$epiU), fill = "pink", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = nov30B$epiL, ymax = nov30B$epiU), fill = "lightblue1", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 55)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# intro may2025
png(paste(outDir,"beh_vac0ind05may_14d_EarlierLater_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,   color="As in 2022",      linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05la, color="14 days later",   linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05ea, color="14 days earlier", linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 40)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# intro nov2025
png(paste(outDir,"beh_vac0ind05nov_14d_EarlierLater_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind05,   color="As in 2022",      linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05la, color="14 days later",   linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05ea, color="14 days earlier", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 40)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# intro may2025
png(paste(outDir,"beh_vac0ind05may_lowerHigherRed_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,   color="As in 2022",            linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05lr, color="25% lower reductions",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05hr, color="25% higher reductions", linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 25)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# intro nov2025
png(paste(outDir,"beh_vac0ind05nov_lowerHigherRed_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind05,   color="As in 2022",            linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05lr, color="25% lower reductions",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05hr, color="25% higher reductions", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 25)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
# intro may2025
png(paste(outDir,"beh_vac0ind05may_y10_y20_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexA,y=c20250501vac0ind05,    color="As in 2022",     linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05y10, color="When >10 cases", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=c20250501vac0ind05y20, color="When >20 cases", linetype="1 May 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 20)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# intro nov2025
png(paste(outDir,"beh_vac0ind05nov_y10_y20_legend.png",sep="/")) 
ggplot(data=beh)+ 
  geom_line(aes(x=datexB,y=c20251101vac0ind05,    color="As in 2022",     linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05y10, color="When >10 cases", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=c20251101vac0ind05y20, color="When >20 cases", linetype="1 Nov 2025" ),size=1) +
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 20)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

######################################################
#
# PLOTS WITH CRED INTERVALS
# 
# scenarios with different behavioural changes
# -- 14d earlier/later than in 2022
# -- 25% higher/lower reductions than in 2022
#
# Run for scen without vac in 2024-2025, intro 5 cases on 1may/1nov2025
######################################################

outDir <- "output/2025_beh2025"

a <- read.table(file = "output/2025_beh2022/nSymp20250501vac0ind05.txt",           header = TRUE, sep = "\t",stringsAsFactors = TRUE)
b <- read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_14dLater.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
c <- read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_14dEarlier.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
d <- read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_lowerRed.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
e <- read.table(file = "output/2025_beh2025/nSymp20250501vac0ind05_higherRed.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)

f <- read.table(file = "output/2025_beh2022/nSymp20251101vac0ind05.txt",           header = TRUE, sep = "\t",stringsAsFactors = TRUE)
g <- read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_14dLater.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
h <- read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_14dEarlier.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)
j <- read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_lowerRed.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
k <- read.table(file = "output/2025_beh2025/nSymp20251101vac0ind05_higherRed.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)

A <- statsFun(a)
B <- statsFun(b)
C <- statsFun(c)
D <- statsFun(d)
E <- statsFun(e)
F <- statsFun(f)
G <- statsFun(g)
H <- statsFun(h)
J <- statsFun(j)
K <- statsFun(k)

may <- data.frame(
  datexA,  datexB,
  A$epiM,  A$epiL,  A$epiU,
  B$epiM,  B$epiL,  B$epiU,
  C$epiM,  C$epiL,  C$epiU,
  D$epiM,  D$epiL,  D$epiU,
  E$epiM,  E$epiL,  E$epiU,
  F$epiM,  F$epiL,  F$epiU,
  G$epiM,  G$epiL,  G$epiU,
  H$epiM,  H$epiL,  H$epiU,
  J$epiM,  J$epiL,  J$epiU,
  K$epiM,  K$epiL,  K$epiU  )

# PLOT intro MAY; CHANGES EARLIER/LATER
png(paste(outDir,"CI_beh_vac0ind05may_14d_EarlierLater.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=A$epiM, color="As in 2022",                  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=B$epiM, color="14 days later",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=C$epiM, color="14 days earlier", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = A$epiL, ymax = A$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = B$epiL, ymax = B$epiU), fill = "lightgreen", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = C$epiL, ymax = C$epiU), fill = "pink", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 60)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# PLOT intro NOV; CHANGES EARLIER/LATER
png(paste(outDir,"CI_beh_vac0ind05nov_14d_EarlierLater.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexB,y=F$epiM, color="As in 2022",     linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=G$epiM, color="14 days later",  linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=H$epiM, color="14 days earlier",linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexB,ymin = F$epiL, ymax = F$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexB,ymin = G$epiL, ymax = G$epiU), fill = "lightgreen", alpha = 0.5) + 
  geom_ribbon(aes(x=datexB,ymin = H$epiL, ymax = H$epiU), fill = "pink", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 60)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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

# PLOT intro MAY; CHANGES HIGHER/LOWER
png(paste(outDir,"CI_beh_vac0ind05may_lowerHigherRed.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=A$epiM, color="As in 2022",            linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=D$epiM, color="25% lower reductions",  linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=E$epiM, color="25% higher reductions", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = A$epiL, ymax = A$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = D$epiL, ymax = D$epiU), fill = "mediumpurple3", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = E$epiL, ymax = E$epiU), fill = "khaki", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 35)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"            = "black",
    "25% lower reductions"  = "purple",
    "25% higher reductions" = "orange2")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro NOV; CHANGES EARLIER/LATER
png(paste(outDir,"CI_beh_vac0ind05nov_lowerHigherRed.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexB,y=F$epiM, color="As in 2022",           linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=J$epiM, color="25% lower reductions", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=K$epiM, color="25% higher reductions",linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexB,ymin = F$epiL, ymax = F$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexB,ymin = J$epiL, ymax = J$epiU), fill = "mediumpurple3", alpha = 0.5) + 
  geom_ribbon(aes(x=datexB,ymin = K$epiL, ymax = K$epiU), fill = "khaki", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 35)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"            = "black",
    "25% lower reductions"  = "purple",
    "25% higher reductions" = "orange2")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

######################################################
#
# PLOTS WITH CRED INTERVALS
# 
# scenarios with different behavioural changes
# CHANGE when numb cases >10 or >20
#
# Run for scen without vac in 2024-2025, intro 5 cases on 1may/1nov2025
######################################################

outDir <- "output/2025_changesDependPrev"

a <- read.table(file = "output/2025_beh2022/nSymp20250501vac0ind05.txt",           header = TRUE, sep = "\t",stringsAsFactors = TRUE)
b <- read.table(file = "output/2025_changesDependPrev/nSymp20250501vac0ind05prevY10.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
c <- read.table(file = "output/2025_changesDependPrev/nSymp20250501vac0ind05prevY20.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

f <- read.table(file = "output/2025_beh2022/nSymp20251101vac0ind05.txt",           header = TRUE, sep = "\t",stringsAsFactors = TRUE)
g <- read.table(file = "output/2025_changesDependPrev/nSymp20251101vac0ind05prevY10.txt",  header = TRUE, sep = "\t",stringsAsFactors = TRUE)
h <- read.table(file = "output/2025_changesDependPrev/nSymp20251101vac0ind05prevY20.txt",header = TRUE, sep = "\t",stringsAsFactors = TRUE)

A <- statsFun(a)
B <- statsFun(b)
C <- statsFun(c)
F <- statsFun(f)
G <- statsFun(g)
H <- statsFun(h)

may <- data.frame(
  datexA,  datexB,
  A$epiM,  A$epiL,  A$epiU,
  B$epiM,  B$epiL,  B$epiU,
  C$epiM,  C$epiL,  C$epiU,
  F$epiM,  F$epiL,  F$epiU,
  G$epiM,  G$epiL,  G$epiU,
  H$epiM,  H$epiL,  H$epiU   )

# PLOT intro MAY; CHANGES WHEN Y >10 OR >20
png(paste(outDir,"CI_beh_vac0ind05may_y10_y20.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexA,y=A$epiM, color="As in 2022",     linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=B$epiM, color="When >10 cases", linetype="1 May 2025" ),size=1) +
  geom_line(aes(x=datexA,y=C$epiM, color="When >20 cases", linetype="1 May 2025" ),size=1) +
  geom_ribbon(aes(x=datexA,ymin = A$epiL, ymax = A$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = B$epiL, ymax = B$epiU), fill = "pink", alpha = 0.5) + 
  geom_ribbon(aes(x=datexA,ymin = C$epiL, ymax = C$epiU), fill = "lightskyblue1", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 28)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"     = "black",
    "When >10 cases" = "red",
    "When >20 cases" = "deepskyblue3")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

# PLOT intro NOV; CHANGES WHEN Y >10 OR >20
png(paste(outDir,"CI_beh_vac0ind05nov_y10_y20.png",sep="/")) 
ggplot(data=may)+ 
  geom_line(aes(x=datexB,y=F$epiM, color="As in 2022",     linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=G$epiM, color="When >10 cases", linetype="1 Nov 2025" ),size=1) +
  geom_line(aes(x=datexB,y=H$epiM, color="When >20 cases", linetype="1 Nov 2025" ),size=1) +
  geom_ribbon(aes(x=datexB,ymin = F$epiL, ymax = F$epiU), fill = "gray", alpha = 0.5) + 
  geom_ribbon(aes(x=datexB,ymin = G$epiL, ymax = G$epiU), fill = "pink", alpha = 0.5) + 
  geom_ribbon(aes(x=datexB,ymin = H$epiL, ymax = H$epiU), fill = "lightskyblue1", alpha = 0.5) + 
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(-1, 28)) +
  theme_light() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  xlab("Date") +
  ylab("Daily number mpox cases") +
  scale_color_manual(values = c(
    "As in 2022"     = "black",
    "When >10 cases" = "red",
    "When >20 cases" = "deepskyblue3")) +
  labs(color = 'Behavioural adaptations') +
  scale_linetype_manual("Introductions",values=c("1 May 2025"="solid","1 Nov 2025"="twodash"))+
  theme(legend.position = 'bottom',legend.direction = "vertical")
dev.off()

################################## END ###################