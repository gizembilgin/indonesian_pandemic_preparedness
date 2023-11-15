

run_disease_model <- function(time_horizon = 365,
                              vaccination_history = data.frame(),
                              
                              this_inital_state = inital_state,
                              this_configure_ODEs = configure_ODEs,
                              this_parameters = parameters,
                              this_age_group_labels = age_group_labels) {
  
  
  state = c(this_inital_state$individuals,
            rep(0,nrow(this_inital_state)/4)) #add empty rows for incidence tracker
  
  if (nrow(vaccination_history) == 0){
    sol = as.data.frame(ode(y=state,
                            times=seq(0,time_horizon,by=1),
                            func=this_configure_ODEs,
                            parms=this_parameters))
  } else{
    #first 100 days of the outbreak, NB: COMEBACK to allow variation in time of detection
    sol_without_vaccine = as.data.frame(ode(y=state,
                            times=seq(0,min(vaccination_history$time),by=1),
                            func=this_configure_ODEs,
                            parms=this_parameters))
    
    #delivery exclusively to essential workers
    sol_delivery_to_essential_workers
    
    #delivery to else (nb: run multiple times depending on vax strategies being tested)
  }

  
  ### TRANSLATE sol of ODEs into incidence_log
  incidence_log_unedited <- sol[,c(1,(nrow(this_inital_state)+2):ncol(sol))]
  
  key = crossing(comorbidity = c(0,1),
                 vaccination_status = c(0,1),
                 age_group = this_age_group_labels) %>%
    mutate(dummy = row_number())
  
  incidence_log_tidy <- incidence_log_unedited %>%
    pivot_longer(cols = c(2:ncol(incidence_log_unedited)),
                 values_to = "incidence",
                 names_to = "dummy") %>%
    mutate(dummy = as.numeric(dummy) - nrow(this_inital_state)) %>%
    left_join(key,by="dummy") %>%
    select(-dummy)
  
  return(incidence_log_tidy)
}
