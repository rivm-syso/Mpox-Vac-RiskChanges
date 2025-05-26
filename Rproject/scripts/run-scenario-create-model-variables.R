#D1-run-selectedParam.r
if(scenario %in% c('6.a.','6.b.')){
  # CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
  #---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS
  
  nSympMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  vac1dMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  vac2dMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  pImm1Matrix  <- matrix(nrow=nsim,ncol=length(xt))
  pImm2Matrix  <- matrix(nrow=nsim,ncol=length(xt))
  pImm3Matrix  <- matrix(nrow=nsim,ncol=length(xt))
  statesEndSel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization  
}

#E1-run-2024-2025-noInf-noVac.r
if(scenario %in% c('7')){
  # CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
  #---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS
  
  pImm1Matrix  <- matrix(nrow=nsim,ncol=length(xt)) # %immune in each risk group
  pImm2Matrix  <- matrix(nrow=nsim,ncol=length(xt))
  pImm3Matrix  <- matrix(nrow=nsim,ncol=length(xt))
  
  # state variable at time points next analyses will start
  # 487=1-5-2025; 668=1-11-2025
  states487Sel <- matrix(nrow=nsim,ncol=nv)
  states668Sel <- matrix(nrow=nsim,ncol=nv)
}

#F1-run-2024-2025.r, F1-run-2024-2025other.r, F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('8.a.','8.b.')){
  # CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
  #---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS
  nSympMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  numbYMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  statesEndSel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization
}

#F1-run-2024-2025.r, F1-run-2024-2025other.r, F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('10.a.','10.b.','11.a.','11.b.','14.a.','14.b.' ,'15.a.','15.b.', '21.a.', '21.b.', '21.c.', '21.d.', '23.a.', '23.b.', '23.c.')){
  # CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
  #---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS
  nSympMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  statesEndSel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization
}

#E3-run-2024-2025-noInf-vac2024.r
if(scenario %in% c('10','11','14','15')){
  # CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
  #---> ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS  
  states487Sel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization
  states668Sel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization  
}

#F1-run-2024-2025-newclade.r
if(scenario %in% c('A3')){
  #---------------------------------------------------------
  # CREATE MATRICES (nsim x [Tmax+1]) to keep results of all INDIV RUNS & each day
  #     ROWS = INDIV SIM RUNS. COLUMNS = TIME UNITS
  #---------------------------------------------------------
  nSympMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  hospiMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  deathMatrix  <- matrix(nrow=nsim,ncol=length(xt))
  statesEndSel <- matrix(nrow=nsim,ncol=nv) # ncol=#modelVariables defined in initialization
}