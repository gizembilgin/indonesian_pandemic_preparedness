

run_disease_model <- function(time_horizon = 365,
                              vaccination_history = data.frame(),
                              
                              this_inital_state = inital_state,
                              this_configure_ODEs = configure_ODEs,
                              this_parameters = parameters,
                              this_age_group_labels = age_group_labels) {
  
  
  state = c(this_inital_state$individuals) 
  skeleton_state <- this_inital_state %>% select(-individuals)
  
  if (nrow(vaccination_history) == 0){
    sol_log = as.data.frame(ode(y=state,
                            times=seq(0,time_horizon,by=1),
                            func=this_configure_ODEs,
                            parms=this_parameters))
  } else{
    
    ### PART 1/3: First 100 days of the outbreak, NB: COMEBACK to allow variation in time of detection
    sol_log = sol_without_vaccine = as.data.frame(ode(y=state,
                            times=seq(0,min(vaccination_history$time),by=1),
                            func=this_configure_ODEs,
                            parms=this_parameters))
    #NB: ncol(sol_without_vaccine) == 101 == 20 * (S + E + I + R + incidence)
    
    
    
    ### PART 2/3: Delivery exclusively to essential workers
    time_sequence <- seq(max(sol_without_vaccine$time)+1, min(vaccination_history$time[vaccination_history$phase != "essential_workers"]) - 1, by = 1)
    
    for (this_time in time_sequence){
      
      #reconstruct 'tidy' state
      state_working = tail.matrix(sol, 1)
      state_working = select(state_working, -time) #remove column with time
      state_working = t(state_working)
      colnames(state_working) <- c("individuals")
      
      prev_state = cbind(skeleton_state,state_working)
      if(abs(sum(prev_state$individuals[prev_state$class != "Incid"]) - sum(this_inital_state$individuals))>1){stop(paste("prev state at time step",this_time,"not equal to inital population size!"))}
      
      todays_vaccinations <- vaccination_history %>%
        filter(time == this_time) %>%
        select(-time,-phase)
      
      todays_vaccinations_by_class <- prev_state %>%
        filter(class != "Incid" & vaccination_status == 0) %>%
        left_join(todays_vaccinations, by = c("age_group","comorbidity")) %>%
        mutate(doses_delivered = case_when(
          is.na(doses_delivered) ~ 0,
          TRUE ~ doses_delivered
        )) %>%
        pivot_wider(names_from = "class",
                    values_from = "individuals") %>%
        mutate(denominator = S + E + I + R) %>%
        pivot_longer(cols = c("S","E","I","R"),
                     names_to = "class",
                     values_to = "individuals") %>%
        mutate(doses_delivered = case_when(
          denominator == 0 ~ 0,
          TRUE ~ doses_delivered * (individuals/denominator))) %>%
        select(-vaccination_status,-denominator,-individuals)
      if(abs(sum(todays_vaccinations$doses_delivered)-sum(todays_vaccinations_by_class$doses_delivered))>1){stop("total doses in todays_vaccinations and todays_vaccinations_by_class do not line up")}
      
      next_state <- prev_state %>%
        filter(class != "Incid") %>%
        left_join(todays_vaccinations_by_class, by = join_by(class, age_group, comorbidity), relationship = "many-to-many") %>% #vaccination status is many to many
        mutate(individuals = case_when(
          vaccination_status == 0 ~ individuals - doses_delivered,
          vaccination_status == 1 ~ individuals + doses_delivered
        )) %>%
        select(-doses_delivered)
      if(abs(sum(next_state$individuals[next_state$class != "Incid"]) - sum(this_inital_state$individuals))>1){stop(paste("next state at time step",this_time,"not equal to inital population size!"))}
      if(nrow(next_state[round(next_state$individuals)<0,])>0){stop("negative individuals in next_state compartments")}
      rm(prev_state,todays_vaccinations,todays_vaccinations_by_class,state_working)
      
      #order correctly
      next_state$class <- factor(next_state$class, levels = c("S","E","I","R"))
      next_state$age_group <- factor(next_state$age_group, levels = age_group_labels)
      next_state <- next_state %>%
        arrange(class,comorbidity,vaccination_status,age_group)  
      
      #vectorise
      next_state <- c(as.numeric(next_state$individuals),
                 rep(0,nrow(next_state[next_state$class == "S",]))) #blank Incidence tracker
      if(abs(sum(next_state) - sum(this_inital_state$individuals))>1){stop(paste("next state at time step",this_time,"not equal to inital population size!"))}
      
      #next time step!
      sol <- as.data.frame(ode(y=next_state,
                               times=(seq(0,1,by=1)),
                               func=this_configure_ODEs,
                               parms=this_parameters))
      
      #label phase
      if (this_time < min(vaccination_history$time[vaccination_history$phase != "essential_workers"])){
        sol <- sol %>% mutate(phase = "essential_workers")
      } else{
        sol <- sol %>% mutate(phase = ####)
      }
      
      #collect
      sol[,1] <- sol[,1]+1*(this_time-1) #make times correct
      sol <- sol[2,] #remove interim next_state with 0 rows for incidence
      sol_log <- rbind(sol_log,sol)
      
      #only run essential workers the first time (nb: run multiple times depending on vax strategies being tested)
      if (this_time == min(vaccination_history$time[vaccination_history$phase != "essential_workers"])-1){
        sol_delivery_to_essential_workers <- sol
      }
    }
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
