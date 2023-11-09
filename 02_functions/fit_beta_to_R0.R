

fit_beta_to_R0 <- function(R0_to_fit = 2,
                          
                           this_average_symptomatic_period = average_symptomatic_period,
                           this_prevalence_symptoms = prevalence_symptoms,
                           this_reduced_infectiousness_asymptomatic = reduced_infectiousness_asymptomatic,
                           this_susceptibility = susceptibility,
                           
                           this_contact_matrix = loaded_setting_characteristics$contact_matrix,
                           this_pop = loaded_setting_characteristics$population,
                           age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")) {
  
  num_age_groups = length(age_group_labels)

  contact_matrix_adjust = matrix(data = 0, 
                                 nrow = num_age_groups, 
                                 ncol = num_age_groups)
  for (i in 1:num_age_groups){
    for (j in 1:num_age_groups){
      contact_matrix_adjust[i,j] = this_contact_matrix[i,j] * this_pop$individuals[i]/this_pop$individuals[j]
    }
  }
  
  #(B) ballpark beta
  contact_sum = rowSums(this_contact_matrix[,1:ncol(this_contact_matrix)])
  contact_ave=sum(contact_sum*(this_pop$individuals/sum(this_pop$individuals))) 
  beta_ball_park = R0_to_fit/(contact_ave*this_average_symptomatic_period)
  
  #(C) fit!
  minimise_this <- function(beta) {
    diag_matrix = beta*this_average_symptomatic_period*(this_prevalence_symptoms+(1-this_prevalence_symptoms)*this_reduced_infectiousness_asymptomatic)
    diag_matrix = diag(diag_matrix,num_age_groups)
    diag_matrix = this_susceptibility*diag_matrix
    
    NGM_R0 <- contact_matrix_adjust %*% diag_matrix
    R0_beta <- abs(eigen(NGM_R0)$values[1])
    
    fit = abs(R0_to_fit-R0_beta)
    
    return(fit)
  }
  
  #(D) check fit!
  beta_optimised = optimize(minimise_this,c(beta_ball_park*1/4,beta_ball_park*4))$minimum
  beta_optimised = rep(beta_optimised,num_age_groups)
  
  beta_check = beta_optimised[1]
  diag_matrix = beta_check*this_average_symptomatic_period*(this_prevalence_symptoms+(1-this_prevalence_symptoms)*this_reduced_infectiousness_asymptomatic)
  diag_matrix = diag(diag_matrix,num_age_groups)
  diag_matrix = this_susceptibility*diag_matrix
  NGM_R0 <- contact_matrix_adjust %*% diag_matrix
  R0_beta <- abs(eigen(NGM_R0)$values[1])
  
  if (! round(R0_beta,digits = 2) == round(R0_to_fit,digits = 2)){stop('beta fitting is not working!')}
  
  return(beta_optimised[1])
  
}