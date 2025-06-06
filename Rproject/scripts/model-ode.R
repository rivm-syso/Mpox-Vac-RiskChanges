##########################################################
#
# MPOX MODEL 
#
# Define system ODEs that describe the model as function
##########################################################

xode <- function(time, states, parameters){
  if(!exists("scenario")){
    warning("No scenario defined for model function")
  }
  with(as.list(c(parameters,states)),{
    
    # PARAM THAT CHANGE DURING THE RUN, at time points T1,T2 (eg DAY 56=21JUNE & 75=10JULY)
    #--------------------------------------
    if(scenario %in% c('21.a.','21.b.')){
      #modelODE2024novac_EarlierLater.r
      if (time < T1+Tchange) {
        aC <- c(aC[1],aC[2],aC3)
        gamma <- 1/tis1
      } else if (time < T2+Tchange) {
        aC <- c((1-D1L)*aC[1],(1-D1M)*aC[2],(1-D1H)*aC3)
        gamma <- 1/tis2
      } else if (time >= T2+Tchange) {
        aC <- c((1-D2L)*aC[1],(1-D2M)*aC[2],(1-D2H)*aC3)
        gamma <- 1/tis3
      }
    }else if(scenario %in% c('21.c.')){
      #modelODE2024novac_lowerRed.r
      if (time < T1) {
        aC <- c(aC[1],aC[2],aC3)
        gamma <- 1/tis1
      } else if (time < T2) {
        aC <- c((1-r25*D1L)*aC[1],(1-r25*D1M)*aC[2],(1-r25*D1H)*aC3)
        gamma <- 1/(tis2*(1+r25))
      } else if (time >= T2) {
        aC <- c((1-r25*D2L)*aC[1],(1-r25*D2M)*aC[2],(1-r25*D2H)*aC3)
        gamma <- 1/(tis3*(1+r25))
      }
    }else if(scenario %in% c('21.d.')){
      #modelODE2024novac_higherRed.r  
      if (time < T1) {
        aC <- c(aC[1],aC[2],aC3)
        gamma <- 1/tis1
      } else if (time < T2) {
        aC <- c((1-r25*D1L)*aC[1],(1-r25*D1M)*aC[2],(1-r25*D1H)*aC3)
        gamma <- 1/(tis2*(1-rt25))
      } else if (time >= T2) {
        aC <- c((1-r25*D2L)*aC[1],(1-r25*D2M)*aC[2],(1-r25*D2H)*aC3)
        gamma <- 1/(tis3*(1-rt25))
      }
      
    }else if(scenario %in% c('23.a.','23.b.','23.c.')){
      #modelODE2024novac-JB-changeDependPrev.r   
      if (flag == 0) {
        aC <- c(aC[1],aC[2],aC3)
        gamma <- 1/tis1
      } else {
        aC <- c((1-D2L)*aC[1],(1-D2M)*aC[2],(1-D2H)*aC3)
        gamma <- 1/tis3
      }  
    } else{ # Basic structure
      if (time < T1) {
        aC <- c(aC[1],aC[2],aC3)
        gamma <- 1/tis1
      } else if (time < T2) {
        aC <- c((1-D1L)*aC[1],(1-D1M)*aC[2],(1-D1H)*aC3)
        gamma <- 1/tis2
      } else if (time >= T2) {
        aC <- c((1-D2L)*aC[1],(1-D2M)*aC[2],(1-D2H)*aC3)
        gamma <- 1/tis3
      }
    }
    
    #transm prob per main part as fcn of number_acts & trans.prob per act
    #--------------------------------------------------------------------
    B <- matrix(nrow=ag,ncol=ag) 
    for(i in 1:ag) {
      for(j in 1:ag) {
        B[i,j]<- 1-(1-beta)^((u[i]+u[j])/2)  }}
    
    # names of the states
    #----------------------------
    S0 <- states[grepl("S0", names(states))] 
    E0 <- states[grepl("E0", names(states))]
    I0 <- states[grepl("I0", names(states))]
    Y0 <- states[grepl("Y0", names(states))]
    R0 <- states[grepl("R0", names(states))] 
    S1 <- states[grepl("S1", names(states))] 
    E1 <- states[grepl("E1", names(states))]
    I1 <- states[grepl("I1", names(states))]
    Y1 <- states[grepl("Y1", names(states))]
    R1 <- states[grepl("R1", names(states))] 
    S2 <- states[grepl("S2", names(states))] 
    E2 <- states[grepl("E2", names(states))]
    I2 <- states[grepl("I2", names(states))]
    Y2 <- states[grepl("Y2", names(states))]
    R2 <- states[grepl("R2", names(states))] 
    
    #define matrices for the states
    #------------------------------------------
    dS0  <- matrix(xstate0[        1:ag],     nrow=ag,ncol=1)
    dE0  <- matrix(xstate0[ (  ag+1):( 2*ag)],nrow=ag,ncol=1)
    dI0  <- matrix(xstate0[ (2*ag+1):( 3*ag)],nrow=ag,ncol=1)
    dY0  <- matrix(xstate0[ (3*ag+1):( 4*ag)],nrow=ag,ncol=1)
    dR0  <- matrix(xstate0[ (4*ag+1):( 5*ag)],nrow=ag,ncol=1)
    dS1  <- matrix(xstate0[ (5*ag+1):( 6*ag)],nrow=ag,ncol=1)
    dE1  <- matrix(xstate0[ (6*ag+1):( 7*ag)],nrow=ag,ncol=1)
    dI1  <- matrix(xstate0[ (7*ag+1):( 8*ag)],nrow=ag,ncol=1)
    dY1  <- matrix(xstate0[ (8*ag+1):( 9*ag)],nrow=ag,ncol=1)
    dR1  <- matrix(xstate0[ (9*ag+1):(10*ag)],nrow=ag,ncol=1)
    dS2  <- matrix(xstate0[(10*ag+1):(11*ag)],nrow=ag,ncol=1)
    dE2  <- matrix(xstate0[(11*ag+1):(12*ag)],nrow=ag,ncol=1)
    dI2  <- matrix(xstate0[(12*ag+1):(13*ag)],nrow=ag,ncol=1)
    dY2  <- matrix(xstate0[(13*ag+1):(14*ag)],nrow=ag,ncol=1)
    dR2  <- matrix(xstate0[(14*ag+1):(15*ag)],nrow=ag,ncol=1) 
    
    #total size of each sex.act group & of each vac.status
    #-----------------------------------------------------
    N0 <- S0+E0+I0+Y0+R0
    N1 <- S1+E1+I1+Y1+R1
    N2 <- S2+E2+I2+Y2+R2
    if(scenario %in% c('7','8','8.a.','8.b.','21.a.','21.b.','21.c.','21.d.','A3')){
      #modelODE2024novac.r, modelODE2024novac_EarlierLater.r, modelODE2024novac_lowerRed.r, modelODE2024novac_higherRed.r, modelODE2024novac_clade.r
      N  <- S0+E0+I0+Y0+R0+S1+E1+I1+Y1+R1+S2+E2+I2+Y2+R2
    } else{ # Basic structure
      #modelODEvac.r, modelODEvac_noTrans.r
      N  <- N0 + N1 + N2
    }
    
    #mixing matrix: m(i,j)=E*d(i,j)+(1-E)*a(j)*N(j)/denom     
    #---------------------------------------------------
    mS   <- matrix(nrow=ag,ncol=ag) # mixing param main partn
    mC   <- matrix(nrow=ag,ncol=ag) # mixing param casual partn
    IDmatrix <- diag(ag)
    denomS <- sum(aS*N) #denominator for mixing param
    denomC <- sum(aC*N)
    mC <- epsilonC*IDmatrix + ((1-epsilonC)/denomC) * t(matrix(rep(aC*N,ag),nrow=ag,ncol=ag))
    mS <- epsilonS*IDmatrix + ((1-epsilonS)/denomS) * t(matrix(rep(aS*N,ag),nrow=ag,ncol=ag))
  
    
    # FOI for casual & steady partners
    #---------------------------------------------------
    foiS <- matrix(nrow=ag,ncol=1) # foi main partn
    foiC <- matrix(nrow=ag,ncol=1) # foi casual partn
    foi  <- matrix(nrow=ag,ncol=1) # foi total
    Z    <- (w*I0 + Y0 + w*I1 + Y1 + w*I2 + Y2)/N
    foiC <- beta *aC * (mC %*% Z)
    foiS <- q  * ((B * mS) %*% Z) 
    foi  <- foiC + foiS
    
    # vaccination rates
    #---------------------------------------------------
    # vacNumDaily = dataframe with rows = daily numbers of 1st or 2nd doses in Group 2 or 3 
    # Row i is day i = {1=25apr2022, ..., 614=31dec2023}
    if(scenario %in% c('7','8','8.a.','8.b.','21.a.','21.b.','21.c.','21.d.','23.a.','23.b.','23.c.','A3')){
      #modelODE2024novac.r, modelODE2024novac_EarlierLater.r, modelODE2024novac_lowerRed.r, modelODE2024novac_higherRed.r, modelODE2024novac-JB-changeDependPrev.r, modelODE2024novac_clade.r   
      phi1 <- c(0,0,0)
      phi2 <- c(0,0,0)
    }else if(scenario %in% c('10','10.a.','10.b.')){
      #modelODEvac2024A.r
      index <- floor(time)+1
      vacRateDose1Groep1 <- dataVacNumDaily2024$FirstDosesAG1[index]/S0[1]
      vacRateDose1Groep2 <- dataVacNumDaily2024$FirstDosesAG2[index]/S0[2]
      vacRateDose1Groep3 <- dataVacNumDaily2024$FirstDosesAG3[index]/S0[3]
      vacRateDose2Groep1 <- dataVacNumDaily2024$SecondDosesAG1[index]/S1[1]
      vacRateDose2Groep2 <- dataVacNumDaily2024$SecondDosesAG2[index]/S1[2]
      vacRateDose2Groep3 <- dataVacNumDaily2024$SecondDosesAG3[index]/S1[3]
      phi1 <- c(vacRateDose1Groep1,vacRateDose1Groep2,vacRateDose1Groep3)
      phi2 <- c(vacRateDose2Groep1,vacRateDose2Groep2,vacRateDose2Groep3)  
    }else if(scenario %in% c('11','11.a.','11.b.')){
      #modelODEvac2024B.r
      index <- floor(time)+1
      vacRateDose1Groep1 <- dataVacNumDaily2024$FirstDosesBG1[index]/S0[1]
      vacRateDose1Groep2 <- dataVacNumDaily2024$FirstDosesBG2[index]/S0[2]
      vacRateDose1Groep3 <- dataVacNumDaily2024$FirstDosesBG3[index]/S0[3]
      vacRateDose2Groep1 <- dataVacNumDaily2024$SecondDosesBG1[index]/S1[1]
      vacRateDose2Groep2 <- dataVacNumDaily2024$SecondDosesBG2[index]/S1[2]
      vacRateDose2Groep3 <- dataVacNumDaily2024$SecondDosesBG3[index]/S1[3]
      phi1 <- c(vacRateDose1Groep1,vacRateDose1Groep2,vacRateDose1Groep3)
      phi2 <- c(vacRateDose2Groep1,vacRateDose2Groep2,vacRateDose2Groep3)
    }else if(scenario %in% c('14','14.a.','14.b.')){  
      #modelODEvac2024A10.r
      index <- floor(time)+1
      vacRateDose1Groep1 <- dataVacNumDaily2024$FirstDosesAG1[index]/S0[1]
      vacRateDose1Groep2 <- dataVacNumDaily2024$FirstDosesAG2[index]/S0[2]
      vacRateDose1Groep3 <- dataVacNumDaily2024$FirstDosesAG3[index]/S0[3]
      vacRateDose2Groep1 <- dataVacNumDaily2024$SecondDosesAG1[index]/S1[1]
      vacRateDose2Groep2 <- dataVacNumDaily2024$SecondDosesAG2[index]/S1[2]
      vacRateDose2Groep3 <- dataVacNumDaily2024$SecondDosesAG3[index]/S1[3]
      phi1 <- 10* c(vacRateDose1Groep1,vacRateDose1Groep2,vacRateDose1Groep3)
      phi2 <- 10* c(vacRateDose2Groep1,vacRateDose2Groep2,vacRateDose2Groep3)  
    }else if(scenario %in% c('15','15.a.','15.b.')){  
      #modelODEvac2024B10.r
      index <- floor(time)+1
      vacRateDose1Groep1 <- dataVacNumDaily2024$FirstDosesBG1[index]/S0[1]
      vacRateDose1Groep2 <- dataVacNumDaily2024$FirstDosesBG2[index]/S0[2]
      vacRateDose1Groep3 <- dataVacNumDaily2024$FirstDosesBG3[index]/S0[3]
      vacRateDose2Groep1 <- dataVacNumDaily2024$SecondDosesBG1[index]/S1[1]
      vacRateDose2Groep2 <- dataVacNumDaily2024$SecondDosesBG2[index]/S1[2]
      vacRateDose2Groep3 <- dataVacNumDaily2024$SecondDosesBG3[index]/S1[3]
      phi1 <- 10* c(vacRateDose1Groep1,vacRateDose1Groep2,vacRateDose1Groep3)
      phi2 <- 10* c(vacRateDose2Groep1,vacRateDose2Groep2,vacRateDose2Groep3)
    } else{ # Basic structure
      index <- floor(time)+1
      vacRateDose1Groep1 <- dataVacNumDaily$FirstDosesG1[index]/S0[1]
      vacRateDose1Groep2 <- dataVacNumDaily$FirstDosesG2[index]/S0[2]
      vacRateDose1Groep3 <- dataVacNumDaily$FirstDosesG3[index]/S0[3]
      vacRateDose2Groep1 <- dataVacNumDaily$SecondDosesG1[index]/S1[1]
      vacRateDose2Groep2 <- dataVacNumDaily$SecondDosesG2[index]/S1[2]
      vacRateDose2Groep3 <- dataVacNumDaily$SecondDosesG3[index]/S1[3]
      phi1 <- c(vacRateDose1Groep1,vacRateDose1Groep2,vacRateDose1Groep3)
      phi2 <- c(vacRateDose2Groep1,vacRateDose2Groep2,vacRateDose2Groep3)
    }
    
    # ODEs 
    #--------------
    # The last 2 terms are the TRANSITIONS BETWEEN RISK GROUPS 
    # G= matrix with transition probab & zeros in the diagonal
    # dS0j =... + g_{kj} S0k + g_{Lj} S0L -(g_{jk} + g_{jL}) * S0j
    if(scenario %in% c('6.b.')){
      #modelODEvac_noTrans.r
      dS0 <- -(foi+phi1+mu)*S0+mu*N          
      dE0 <-   foi*S0-(theta+mu)*E0         
      dI0 <- theta*E0-(delta+mu)*I0          
      dY0 <- delta*I0-(gamma+zeta+mu+muD)*Y0
      dR0 <- (gamma+zeta)*Y0-mu*R0           
      
      dS1 <- -(1-sigma1)*foi-(phi2+mu)*S1+phi1*S0+psi*S2 
      dE1 <-  (1-sigma1)*foi*S1-(theta+mu)*E1            
      dI1 <-  (1-sigma1)*theta*E1-(delta+mu)*I1          
      dY1 <- delta*I1-(gamma+zeta+mu+muD)*Y1             
      dR1 <- (gamma+zeta)*Y1+sigma1*theta*E1-mu*R1      
      
      dS2 <- -(1-sigma2)*foi*S2-(psi+mu)*S2+phi2*S1 
      dE2 <-  (1-sigma2)*foi*S2-(theta+mu)*E2       
      dI2 <-  (1-sigma2)*theta*E2-(delta+mu)*I2     
      dY2 <- delta*I2-(gamma+zeta+mu+muD)*Y2       
      dR2 <- (gamma+zeta)*Y2+sigma2*theta*E2-mu*R2    
      
    } else{ # Basic structure
      dS0 <- -(foi+phi1+mu)*S0+mu*N          + t(G) %*% S0 -rowSums(G)*S0
      dE0 <-   foi*S0-(theta+mu)*E0          + t(G) %*% E0 -rowSums(G)*E0
      dI0 <- theta*E0-(delta+mu)*I0          + t(G) %*% I0 -rowSums(G)*I0
      dY0 <- delta*I0-(gamma+zeta+mu+muD)*Y0 + t(G) %*% Y0 -rowSums(G)*Y0
      dR0 <- (gamma+zeta)*Y0-mu*R0           + t(G) %*% R0 -rowSums(G)*R0
      
      dS1 <- -(1-sigma1)*foi-(phi2+mu)*S1+phi1*S0+psi*S2 + t(G) %*% S1 -rowSums(G)*S1
      dE1 <-  (1-sigma1)*foi*S1-(theta+mu)*E1            + t(G) %*% E1 -rowSums(G)*E1
      dI1 <-  (1-sigma1)*theta*E1-(delta+mu)*I1          + t(G) %*% I1 -rowSums(G)*I1
      dY1 <- delta*I1-(gamma+zeta+mu+muD)*Y1             + t(G) %*% Y1 -rowSums(G)*Y1
      dR1 <- (gamma+zeta)*Y1+sigma1*theta*E1-mu*R1       + t(G) %*% R1 -rowSums(G)*R1
      
      dS2 <- -(1-sigma2)*foi*S2-(psi+mu)*S2+phi2*S1 + t(G) %*% S2 -rowSums(G)*S2
      dE2 <-  (1-sigma2)*foi*S2-(theta+mu)*E2       + t(G) %*% E2 -rowSums(G)*E2
      dI2 <-  (1-sigma2)*theta*E2-(delta+mu)*I2     + t(G) %*% I2 -rowSums(G)*I2
      dY2 <- delta*I2-(gamma+zeta+mu+muD)*Y2        + t(G) %*% Y2 -rowSums(G)*Y2
      dR2 <- (gamma+zeta)*Y2+sigma2*theta*E2-mu*R2  + t(G) %*% R2 -rowSums(G)*R2
    }
    
    if(scenario %in% c('23.a.','23.b.','23.c.')){
      # JB: flag is a constant, but should be part of state variables that are integrated
      # setting the derivative to 0 is a trick to have a constant state variable
      # that can be changed during an event
      dflag <- 0       
    }
    
    # new infections & incidence 
    inf  <- matrix(nrow=ag,ncol=1)  # new infections in each activ group
    sym  <- matrix(nrow=ag,ncol=1)  # cases in each activ group
    vac1 <- matrix(nrow=ag,ncol=1)  # vac in each sex.act.group (rows) & each dose (col)
    vac2 <- matrix(nrow=ag,ncol=1)  # vac in each sex.act.group (rows) & each dose (col)
    Imm  <- matrix(nrow=ag,ncol=1)  # vac in each sex.act.group (rows) & each dose (col)
    Ng   <- matrix(nrow=ag,ncol=1)  # vac in each sex.act.group (rows) & each dose (col)
    
    inf <- foi*(S0+(1-sigma1)*S1+(1-sigma2)*S2)
    inc <- 100000*sum(inf)/sum(N)
    sym <- delta*sum(I0+I1+I2) #total num cases in all sex.act.groups
    vac1 <- sum(phi1*S0) #total 1st doses in all sex.act.groups
    vac2 <- sum(phi2*S1) # total 2nd doses in all sex.act.groups
    
    if(scenario %in% c('7','8','8.a.','8.b.')){
      hospi <- zeta*sum(Y0+Y1+Y2)
      death <-  muD*sum(Y0+Y1+Y2)
      nY    <-      sum(Y0+Y1+Y2)
    }else if(scenario %in% c('A3')){
      #modelODE2024novac_clade.r
      hospi <- zetaClade*gamma*sum(Y0+Y1+Y2)
      death <-  muD*sum(Y0+Y1+Y2)
      nY    <-      sum(Y0+Y1+Y2)
    }
    
    Imm1 <- S11+S21 + R01+R11+R21
    Imm2 <- S12+S22 + R02+R12+R22
    Imm3 <- S13+S23 + R03+R13+R23
    Imm  <- c(Imm1,Imm2,Imm3)
    
    Ng1 <- Imm1+S01+E01+E11+E21+I01+I11+I21+Y01+Y11+Y21
    Ng2 <- Imm2+S02+E02+E12+E22+I02+I12+I22+Y02+Y12+Y22
    Ng3 <- Imm3+S03+E03+E13+E23+I03+I13+I23+Y03+Y13+Y23
    Ng  <- c(Ng1,Ng2,Ng3)
    pImm <- 100*Imm/Ng # percentImmune in each risk group
    
    # return data as list
    if(scenario %in% c('7','8','8.a.','8.b.','A3')){
      #modelODE2024novac.r, modelODE2024novac_clade.r
      return(list(c(dS0,dE0,dI0,dY0,dR0,dS1,dE1,dI1,dY1,dR1,dS2,dE2,dI2,dY2,dR2),c(inc=inc,sym=sym,vac1=vac1,vac2=vac2,pImm=pImm,hospi=hospi,death=death,nY=nY)))
    }else if(scenario %in% c('23.a.','23.b.','23.c.')){
      #modelODE2024novac-JB-changeDependPrev.r         
      return( list(c(dS0,dE0,dI0,dY0,dR0,dS1,dE1,dI1,dY1,dR1,dS2,dE2,dI2,dY2,dR2,dflag),c(inc=inc,sym=sym,vac1=vac1,vac2=vac2,pImm=pImm)))
    } else { # Basic structure
      return(list(c(dS0,dE0,dI0,dY0,dR0,dS1,dE1,dI1,dY1,dR1,dS2,dE2,dI2,dY2,dR2),c(inc=inc,sym=sym,vac1=vac1,vac2=vac2,pImm=pImm)))
    }
    
  })}