### This function configures the system of ordinary differential equations (ODEs) for transmission

configure_ODEs <- function(t, state, parameters){

  with(as.list(c(state,parameters)),{
    
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
    total_sequence <- ((1:(num_disease_classes*RISK*(D+1)))-1)*J
    total_infected_mod_sequence <- ((1:(RISK*(D+1)))-1)*J
    for (j in 1:J){
      total = sum(state[total_sequence+j]) #total in contact age j
      total_infected_mod = sum(I[total_infected_mod_sequence+j])        #total infected in contact age j
      tau = tau + contact_matrix[,j]*(total_infected_mod*(lota*(1-gamma[j])+gamma[j]))/(total)
    }
    tau=tau*(1-NPI)*beta*suscept
    tau[tau>1] <- 1 #transmission can not be more than 1 (100%)
    tau[tau<0] <- 0
    

    #construct ODEs
    unvax = 1:J
    for (r in 2:RISK){
      unvax = c(unvax, 
                ((1:(r-1))*J*(D+1)+1):((1:(r-1))*J*(D+1)+J)
      )}
    vax   = unvax + J
    
    dS[unvax] = omega*R[unvax]  - tau*S[unvax] 
    dE[unvax] = tau*S[unvax] - lambda*E[unvax] + tau*(1-rho)*R[unvax]
    dR[unvax] = delta*I[unvax]  - omega*R[unvax]  - tau*(1-rho)*R[unvax]
    
    dS[vax] = omega*R[vax]              - tau*(1-VE)*S[vax] 
    dE[vax] = tau*(1-VE)*S[vax] - lambda*E[vax] + tau*(1-VE)*(1-rho)*R[vax]
    dR[vax] = delta*I[vax]              - omega*R[vax]  - tau*(1-VE)*(1-rho)*R[vax]
    
    dI = lambda*E - delta*I
    dIncidence = lambda*E
    
    dS = as.numeric(dS)
    dE = as.numeric(dE)
    dI = as.numeric(dI)
    dR = as.numeric(dR)
    list(c(dS,dE,dI,dR,dIncidence))
    
  })
}

