#scripts/initialState2022.r
if(scenario %in% c('6.a.','6.b.')){
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
}

#E1-run-2024-2025-noInf-noVac.r
if(scenario %in% c('7')){
  N <- c(0.60,0.35,0.05)*250000
  
  E00 <- rep.int(0,ag)
  I00 <- c(0,0,0)
  Y00 <- rep.int(0,ag)
  E10 <- rep.int(0,ag)
  I10 <- rep.int(0,ag)
  Y10 <- rep.int(0,ag)
  E20 <- rep.int(0,ag)
  I20 <- rep.int(0,ag)
  Y20 <- rep.int(0,ag)  
}

#F1-run-2024-2025.r, F1-run-2024-2025other.r, F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('8.a.','8.b.','10.a.','10.b.','11.a.','11.b.','14.a.','14.b.' ,'15.a.','15.b.', '21.a.', '21.b.', '21.c.', '21.d.', '23.a.', '23.b.', '23.c.')){
  N <- c(0.60,0.35,0.05)*250000
  
  E00 <- rep.int(0,ag)
  #I00 <- c(0,0,3)
  #Y00 <- rep.int(0,ag)
  E10 <- rep.int(0,ag)
  I10 <- rep.int(0,ag)
  Y10 <- rep.int(0,ag)
  E20 <- rep.int(0,ag)
  I20 <- rep.int(0,ag)
  Y20 <- rep.int(0,ag)
}

#E3-run-2024-2025-noInf-vac2024.r
if(scenario %in% c('10','11','14','15')){
  
  N <- c(0.60,0.35,0.05)*250000
  
  E00 <- rep.int(0,ag)
  I00 <- c(0,0,0)
  Y00 <- rep.int(0,ag)
  E10 <- rep.int(0,ag)
  I10 <- rep.int(0,ag)
  Y10 <- rep.int(0,ag)
  E20 <- rep.int(0,ag)
  I20 <- rep.int(0,ag)
  Y20 <- rep.int(0,ag)
}

#F1-run-2024-2025-newclade.r
if(scenario %in% c('A3')){
  N <- c(0.60,0.35,0.05)*250000
  
  E00 <- rep.int(0,ag)
  #I00 <- c(0,0,3)
  #Y00 <- rep.int(0,ag)
  E10 <- rep.int(0,ag)
  I10 <- rep.int(0,ag)
  Y10 <- rep.int(0,ag)
  E20 <- rep.int(0,ag)
  I20 <- rep.int(0,ag)
  Y20 <- rep.int(0,ag)  
}