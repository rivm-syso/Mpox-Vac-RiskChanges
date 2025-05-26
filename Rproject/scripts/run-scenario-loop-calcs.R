# D1-run-selectedParam.r
if(scenario %in% c('6.a.','6.b.')){
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run  
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
  vac1dMatrix[i,] <- with(c(xpar,sol),{sol$vac1}) 
  vac2dMatrix[i,] <- with(c(xpar,sol),{sol$vac2}) 
  pImm1Matrix[i,] <- with(c(xpar,sol),{sol$pImm1}) 
  pImm2Matrix[i,] <- with(c(xpar,sol),{sol$pImm2}) 
  pImm3Matrix[i,] <- with(c(xpar,sol),{sol$pImm3}) 
  for (j in 1:nv) {statesEndSel[i,j]<- with(c(xpar,sol),{sol[Tmax,(j+1)]})}
}

#E1-run-2024-2025-noInf-noVac.r
if(scenario %in% c('7')){
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  pImm1Matrix[i,] <- with(c(xpar,sol),{sol$pImm1}) 
  pImm2Matrix[i,] <- with(c(xpar,sol),{sol$pImm2}) 
  pImm3Matrix[i,] <- with(c(xpar,sol),{sol$pImm3}) 
  for (j in 1:nv) {
    states487Sel[i,j]<- with(c(xpar,sol),{sol[487,(j+1)]}) 
    states668Sel[i,j]<- with(c(xpar,sol),{sol[668,(j+1)]})
  }
}

#F1-run-2024-2025.r
if(scenario %in% c('8.a.','8.b.')){
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
    
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
  numbYMatrix[i,] <- with(c(xpar,sol),{sol$nY}) # (nsim)x(Tmax+1) matrix with all runs,all days
}

#F1-run-2024-2025.r
if(scenario %in% c('10.a.','10.b.','11.a.','11.b.','14.a.','14.b.' ,'15.a.','15.b.')){
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
}

#F1-run-2024-2025other.r
if(scenario %in% c('21.a.', '21.b.', '21.c.', '21.d.')){
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,Fbeta=Fbeta,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD,zetaClade=zetaClade))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
}

#F1-run-2024-2025-JB-changeDependPrev.r
if(scenario %in% c('23.a.', '23.b.', '23.c.')){
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  
  # JB: define stop and go functions for ODE solver
  
  # JB: root function to define the moment the event is triggered, when yroot is zero
  rootfunc <- function(times, y, params) {
    with(as.list(c(params)), {
      limitVar <- sum(y[grepl(x = names(y), pattern = "^Y")])
      yroot <- limitVar - limitValue
      return(yroot)
    })
  }
  
  # JB: the triggered event changes the state variable flag
  eventfunc <- function(times, y, params) {
    with(as.list(c(params)), {
      y["flag"] <- 1
      return(y)
    })
  }
  
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar,
                           rootfun = rootfunc,
                           events = list(func = eventfunc, root = TRUE)))
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
    
}

#E3-run-2024-2025-noInf-vac2024.r
if(scenario %in% c('10','11','14','15')){
  xpar <- as.list(c(dataVacNumDaily2024=dataVacNumDaily2024,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  for (j in 1:nv) {
    states487Sel[i,j]<- with(c(xpar,sol),{sol[487,(j+1)]})
    states668Sel[i,j]<- with(c(xpar,sol),{sol[668,(j+1)]})  
  }

}

#F1-run-2024-2025-newclade.r
if(scenario %in% c('A3')){
  tis1    <-   upSel[i,1]*Finf
  beta    <-   upSel[i,4]*Fbeta
  
  xpar <- as.list(c(dataVacNumDaily=dataVacNumDaily,Fbeta=Fbeta,theta=theta,delta=delta,tis1=tis1,tis2=tis2,tis3=tis3,beta=beta,sigma1=sigma1,sigma2=sigma2,psi=psi,epsilonS=epsilonS,epsilonC=epsilonC,w=w,D1L=D1L,D1M=D1M,D1H=D1H,D2L=D2L,D2M=D2M,D2H=D2H,aC3=aC3,mu=mu,oldVac=oldVac,aS=aS,aC=aC,u=u,q=q,G=G,zeta=zeta,muD=muD,zetaClade=zetaClade))
  sol <- as.data.frame(ode(xstate0, xt, xode, xpar)) #the LAST par.run
  
  nSympMatrix[i,] <- with(c(xpar,sol),{sol$sym}) # (nsim)x(Tmax+1) matrix with all runs,all days
  hospiMatrix[i,] <- with(c(xpar,sol),{sol$hospi}) # (nsim)x(Tmax+1) matrix with all runs,all days
  deathMatrix[i,] <- with(c(xpar,sol),{sol$death}) # (nsim)x(Tmax+1) matrix with all runs,all days
}
