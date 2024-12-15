#################################################
# INITIAL STATES OF MODEL VARIABLES 
#################################################

N <- c(0.60,0.35,0.05)*250000

S00 <- (1 - oldVac)*N
E00 <- rep.int(0,ag)
I00 <- c(0,0,3)
Y00 <- rep.int(0,ag)
R00 <- rep.int(0,ag)

S10 <- oldVac*N
E10 <- rep.int(0,ag)
I10 <- rep.int(0,ag)
Y10 <- rep.int(0,ag)
R10 <- rep.int(0,ag)

S20 <- rep.int(0,ag)
E20 <- rep.int(0,ag)
I20 <- rep.int(0,ag)
Y20 <- rep.int(0,ag)
R20 <- rep.int(0,ag)

xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 

################################## END #########