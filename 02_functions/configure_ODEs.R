### This function configures the system of ordinary differential equations (ODEs) for transmission

configure_ODEs <- function(t, state, parameters){
  require(deSolve)
  
  with(as.list(c(state,parameters)),{
    
    J = num_age_groups
    D = 1 #num_vax_doses
    RISK = 2 #num_risk_groups
    num_disease_classes = 4
    
    A=J*(D+1)*RISK # +1 is unvax
    
    S=state[1:A]
    E=state[(A+1):(2*A)]
    I=state[(2*A+1):(3*A)]
    R=state[(3*A+1):(4*A)]
    
    dS = dE = dI = dR = dIncidence  <- numeric(length=A)
    
    #calculating transmission to each age group
    tau =rep(0,J) 
    for (i in 1:J){
      for (j in 1:J){

        total = sum(state[((1:(num_disease_classes*RISK*(D+1)))-1)*J+j]) #total in contact age j
        total_infected_mod = sum(I    [((1:(RISK*(D+1)))-1)*J+j])        #total infected in contact age j

        tau[i]=tau[i]+contact_matrix[i,j]*(total_infected_mod*(lota*(1-gamma[j])+gamma[j]))/(total)
      }
    }
    
    tau=tau*(1-NPI)*beta*suscept
    tau[tau>1] <- 1 #transmission can not be more than 1 (100%)
    tau[tau<0] <- 0
    
    
    #system of ODEs
    for (r in 1:RISK){
      for (i in 1:J){
        #unvaccinated
        unvax = i + (r-1) *J*(D+1)
        dS[unvax] = omega*R[unvax]  - tau[i]*S[unvax] 
        dE[unvax] = tau[i]*S[unvax] - lambda*E[unvax] + tau[i]*(1-rho)*R[unvax]
        dI[unvax] = lambda*E[unvax] - delta*I[unvax]
        dR[unvax] = delta*I[unvax]  - omega*R[unvax]  - tau[i]*(1-rho)*R[unvax]
        dIncidence[unvax] = lambda*E[unvax]
        
        #vaccinated
        B = unvax + J
        dS[B] = omega*R[B]              - tau[i]*(1-VE)*S[B] 
        dE[B] = tau[i]*(1-VE)*S[B] - lambda*E[B] + tau[i]*(1-VE)*(1-rho)*R[B]
        dI[B] = lambda*E[B]             - delta*I[B]
        dR[B] = delta*I[B]              - omega*R[B]  - tau[i]*(1-VE)*(1-rho)*R[B]
        dIncidence[B] = lambda*E[B] 
      }
    }
    
    dS = as.numeric(dS)
    dE = as.numeric(dE)
    dI = as.numeric(dI)
    dR = as.numeric(dR)
    list(c(dS,dE,dI,dR,dIncidence))
    
  })
}

