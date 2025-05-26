# D1-run-selectedParam.r
if(scenario %in% c('6.a.','6.b.')){
}

#E1-run-2024-2025-noInf-noVac.r
if(scenario %in% c('7')){
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3])
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33])
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 
}

#F1-run-2024-2025.r
if(scenario %in% c('8.a.','8.b.','10.a.','10.b.','11.a.','11.b.','14.a.','14.b.' ,'15.a.','15.b.')){
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3]) - c(vacF1,vacF2,vacF3)
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33]) + c(vacF1,vacF2,vacF3)
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 
}

#F1-run-2024-2025other.r, F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('21.a.', '21.b.', '21.c.', '21.d.')){
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3]) - c(vacF1,vacF2,vacF3)
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33]) + c(vacF1,vacF2,vacF3)
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 
}

#F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('23.a.', '23.b.', '23.c.')){
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3]) - c(vacF1,vacF2,vacF3)
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33]) + c(vacF1,vacF2,vacF3)
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  
  flag <- 0 #JB: add flag to signal change in gamma as a state variable
  
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20,flag=flag) 
}

#E3-run-2024-2025-noInf-vac2024.r
if(scenario %in% c('10','11','14','15')){
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3])
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33])
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20) 
}

#F1-run-2024-2025-newclade.r
if(scenario %in% c('A3')){
  S00 <- c(stateEnded[i, 1],stateEnded[i, 2],stateEnded[i, 3]) - c(vacF1,vacF2,vacF3)
  R00 <- c(stateEnded[i,13],stateEnded[i,14],stateEnded[i,15])
  S10 <- c(stateEnded[i,16],stateEnded[i,17],stateEnded[i,18])
  R10 <- c(stateEnded[i,28],stateEnded[i,29],stateEnded[i,30])
  S20 <- c(stateEnded[i,31],stateEnded[i,32],stateEnded[i,33]) + c(vacF1,vacF2,vacF3)
  R20 <- c(stateEnded[i,43],stateEnded[i,44],stateEnded[i,45])
  xstate0 <- c(S0=S00,E0=E00,I0=I00,Y0=Y00,R0=R00,S1=S10,E1=E10,I1=I10,Y1=Y10,R1=R10,S2=S20,E2=E20,I2=I20,Y2=Y20,R2=R20)   
}