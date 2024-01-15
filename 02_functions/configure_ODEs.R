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
    tau =(rep(0,J)) 
    
    #calculating transmission to each age group
    for (i in 1:J){
      for (j in 1:J){
        
        total=0 #total in contact age j
        for(interval in 1:(num_disease_classes*RISK*(D+1))){
          total = total+state[j+(interval-1)*J] 
        }
        
        total_infected_mod = 0 #total infected in age j: (2 x 2 = 4 compartments (vax/unvax * comorb))
        for (r in 1:RISK){
          total_infected_mod = total_infected_mod + I[j+(r-1)*J*(D+1)] #unvax
          for (d in 1:D){
              B = j + J*(d-1)+(r-1)*J*(D+1)
              #total_infected_mod=total_infected_mod + (1-VE_onwards[t,d])*I[B]
              total_infected_mod=total_infected_mod + I[B]
          }
        }
        tau[i]=tau[i]+contact_matrix[i,j]*(total_infected_mod*(lota*(1-gamma[j])+gamma[j]))/(total)
        
      }
      tau[i]=tau[i]*(1-NPI)*beta*suscept[i]
      tau[i]=max(min(1,tau[i]),0) #transmission can not be more than 1 (100%)
    }
    
    #system of ODEs
    for (r in 1:RISK){
      for (i in 1:J){
        #unvaccinated
        unvax = i + (r-1)*A/RISK
        dS[unvax] = omega*R[unvax]  - tau[i]*S[unvax] 
        dE[unvax] = tau[i]*S[unvax] - lambda*E[unvax] + tau[i]*(1-rho)*R[unvax]
        dI[unvax] = lambda*E[unvax] - delta*I[unvax]
        dR[unvax] = delta*I[unvax]  - omega*R[unvax]  - tau[i]*(1-rho)*R[unvax]
        dIncidence[unvax] = lambda*E[unvax]
        
        #vaccinated
        B = i + J + (r-1)*A/RISK
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



