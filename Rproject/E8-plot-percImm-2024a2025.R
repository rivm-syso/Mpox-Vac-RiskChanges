##########################################################
#
# MPOX model
# Percent immune in group 3 = high risk, 2024-2025
#
##########################################################

Tmax <- 2*365
dateImm  <- seq(as.Date("2024-01-01"), as.Date("2025-12-30"), by="days")

# xt   <- seq(1,Tmax+1,1) # time sequence 
# 
# datexA  <- seq(as.Date("2025-05-01"), as.Date("2025-11-17"), by="days")
# datexB  <- seq(as.Date("2025-11-01"), as.Date("2026-05-20"), by="days")

#------------------------------
# LIBRARIES & own scripts
#------------------------------

library(ggplot2) 
library(reshape2)
library(dplyr)

source("scripts/resEpiFunctions.r")   # Rfunctions for epi outcomes, eg pre,inc

#------------------------------
# Read %immune in group 3 and plot
#------------------------------

percImm3 <- read.table(file = "output/2024a2025_noInf/percImm_group3.txt",header = TRUE,sep = "\t",stringsAsFactors = TRUE)
percImm3 <- statsFun(as.data.frame(percImm3[,-Tmax])) 
percImm3 <- mutate(percImm3, dateImm = dateImm)

png("output/2024a2025_noInf/plot_percImm_group3.png") 
ggplot(data=percImm3)+ 
  geom_line(  aes(x=dateImm, y    = percImm3$epiM), color="blue" ,size=1) +
  geom_ribbon(aes(x=dateImm, ymin = percImm3$epiL,  ymax = percImm3$epiU),  fill = "lightblue", alpha = 0.5) + # Shaded area
  scale_x_date(date_labels = "%d %b %Y",
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "60 days")) +
  scale_y_continuous(limits = c(33,55)) +
  theme_light() +
  theme(text = element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Date") +
  ylab("Daily number mpox cases") 
dev.off()

########################## END #####################################