# D1-run-selectedParam.r
if(scenario %in% c('6.a.','6.b.')){
  #---------------------------------------------------------
  # SAVE model state (S00, S01, etc) at Tmax for selected param
  #---------------------------------------------------------
  
  statesEndSel <- as.data.frame(statesEndSel)
  
  varNames <- colnames(sol)
  colnames(statesEndSel) <- varNames[2:(nv+1)]
  
  write.table(statesEndSel,  
              file=paste(outDir,"model_state_Sel.txt",sep="/"),
              sep="\t",
              row.names=TRUE,
              quote=FALSE )
  
  #---------------------------------------------------------
  # Save nSymp as dataFrame (nsim X [Tmax+1]). 
  # nSymp[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  nSympSel <- as.data.frame(nSympMatrix)
  
  write.table(nSympSel,  
              file=paste(outDir,"nSympSel_WithoutRep.txt",sep="/"),
              sep="\t",
              row.names=TRUE,
              quote=FALSE )
  
  #---------------------------------------------------------
  # Save pImm1,pImm2,pImm3 = %immune in sex act group 1,2,3
  # save  as dataFrame (nsim X [Tmax+1]). 
  # pImm1[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  
  pImm1 <- as.data.frame(pImm1Matrix)
  pImm2 <- as.data.frame(pImm2Matrix)
  pImm3 <- as.data.frame(pImm3Matrix)
  
  write.table(pImm1, file=paste(outDir,"percImm_group1.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  write.table(pImm2, file=paste(outDir,"percImm_group2.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  write.table(pImm3, file=paste(outDir,"percImm_group3.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  
  #------------------------------------------
  # Number of MSM with vaccine protection
  #------------------------------------------
  
  vac1d <- as.data.frame(vac1dMatrix[,2:(Tmax+1)]) #extract 1st col for t=0 =26apr
  vac2d <- as.data.frame(vac2dMatrix[,2:(Tmax+1)]) #extract 1st col for t=0 =26apr
  
  modVacNum <- data.frame(
    vac1daily    = vac1dMatrix[1,2:(Tmax+1)],
    vac2daily    = vac2dMatrix[1,2:(Tmax+1)],
    vac1dailyCum = cumsum(vac1dMatrix[1,2:(Tmax+1)]),
    vac2dailyCum = cumsum(vac2dMatrix[1,2:(Tmax+1)]))
  
  write.table(modVacNum,
              file=paste(outDir,"statsVacsSel.txt",sep="/"),
              sep="\t",
              row.names=TRUE,
              quote=FALSE )  
}

#E1-run-2024-2025-noInf-noVac.r
if(scenario %in% c('7')){
  #---------------------------------------------------------
  # SAVE model state (S00, S01, etc) at Tmax for selected param
  #---------------------------------------------------------
  
  states487Sel <- as.data.frame(states487Sel)
  states668Sel <- as.data.frame(states668Sel)
  
  varNames <- colnames(sol)
  
  colnames(states487Sel) <- varNames[2:(nv+1)]
  colnames(states668Sel) <- varNames[2:(nv+1)]
  
  write.table(states487Sel, file=paste(outDir,"model_state_Sel_487.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  write.table(states668Sel, file=paste(outDir,"model_state_Sel_668.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  
  #---------------------------------------------------------
  # Save pImm1,pImm2,pImm3 = %immune in sex act group 1,2,3
  # save  as dataFrame (nsim X [Tmax+1]). 
  # pImm1[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  
  datex25  <- seq(as.Date("2024-01-01"), as.Date("2025-12-30"), by="days")
  
  pImm1 <- as.data.frame(pImm1Matrix)
  pImm2 <- as.data.frame(pImm2Matrix)
  pImm3 <- as.data.frame(pImm3Matrix)
  
  write.table(pImm1, file=paste(outDir,"percImm_group1.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  write.table(pImm2, file=paste(outDir,"percImm_group2.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  write.table(pImm3, file=paste(outDir,"percImm_group3.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  
  percImm1 <- statsFun(as.data.frame(pImm1[,-Tmax])) 
  percImm2 <- statsFun(as.data.frame(pImm2[,-Tmax])) 
  percImm3 <- statsFun(as.data.frame(pImm3[,-Tmax])) 
  
  percImm1 <- mutate(percImm1,xt =  xt[1:(Tmax)], datex = datex25)
  percImm2 <- mutate(percImm2,xt =  xt[1:(Tmax)], datex = datex25)
  percImm3 <- mutate(percImm3,xt =  xt[1:(Tmax)], datex = datex25)
  
  percImmStats <- data.frame(
    xt     = percImm1$xt[1:(Tmax)],
    datex25= datex25,
    pImm1M = percImm1$epiM,
    pImm1L = percImm1$epiL,
    pImm1U = percImm1$epiU,
    pImm2M = percImm2$epiM,
    pImm2L = percImm2$epiL,
    pImm2U = percImm2$epiU,
    pImm3M = percImm3$epiM,
    pImm3L = percImm3$epiL,
    pImm3U = percImm3$epiU   )
  
  write.table(percImmStats,
              file=paste(outDir,"percImm_all_stats.txt",sep="/"),
              sep="\t",
              row.names=TRUE,
              quote=FALSE )  

}

#F1-run-2024-2025.r
if(scenario %in% c('8.a.','8.b.')){
  #---------------------------------------------------------
  # Save nSymp as dataFrame (nsim X [Tmax+1]). 
  # nSymp[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  nSympSel <- as.data.frame(nSympMatrix)
  numbYSel <- as.data.frame(numbYMatrix)
}

#F1-run-2024-2025.r
if(scenario %in% c('10.a.','10.b.','11.a.','11.b.','14.a.','14.b.' ,'15.a.','15.b.')){
  #---------------------------------------------------------
  # Save nSymp as dataFrame (nsim X [Tmax+1]). 
  # nSymp[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  nSympSel <- as.data.frame(nSympMatrix)
}

#F1-run-2024-2025other.r
if(scenario %in% c('21.a.', '21.b.', '21.c.', '21.d.','A3')){
  #---------------------------------------------------------
  # Save nSymp as dataFrame (nsim X [Tmax+1]). 
  # nSymp[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  nSympSel <- as.data.frame(nSympMatrix)
}

#F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('23.a.', '23.b.', '23.c.')){
  #---------------------------------------------------------
  # Save nSymp as dataFrame (nsim X [Tmax+1]). 
  # nSymp[i,j] = #cases with selected par.set i, on time unit j
  #---------------------------------------------------------
  nSympSel <- as.data.frame(nSympMatrix)
  
  as_tibble(sol) |> 
    ggplot(aes(x = time, y = sym)) +
    geom_line() +
    theme_light()
}

#E3-run-2024-2025-noInf-vac2024.r
if(scenario %in% c('10','11','14','15')){
  #---------------------------------------------------------
  # SAVE model state (S00, S01, etc) at Tmax for selected param
  #---------------------------------------------------------
  
  states487Sel <- as.data.frame(states487Sel)
  states668Sel <- as.data.frame(states668Sel)
  
  varNames <- colnames(sol)
  
  colnames(states487Sel) <- varNames[2:(nv+1)]
  colnames(states668Sel) <- varNames[2:(nv+1)]
  
  write.table(states487Sel, file=paste(outDir,"model_state_Sel_487.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )
  write.table(states668Sel, file=paste(outDir,"model_state_Sel_668.txt",sep="/"), sep="\t", row.names=TRUE, quote=FALSE )  
}