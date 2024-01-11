

run_disease_model <- function(time_horizon = 365,
                              vaccination_history = data.frame(),
                              
                              this_inital_state = inital_state,
                              this_configure_ODEs = configure_ODEs,
                              this_parameters = parameters,
                              this_age_group_labels = age_group_labels) {
  
  
  state = c(this_inital_state$individuals) 
  skeleton_state <- this_inital_state %>% select(-individuals)
  
  
  #first scenario: no vaccines -> run for entire time_horizon
  sol_log = as.data.frame(ode(y=state,
                          times=seq(0,time_horizon,by=1),
                          func=this_configure_ODEs,
                          parms=this_parameters)) %>%
    mutate(phase = "no vaccine",
           supply = 0)
  
  
  if (nrow(vaccination_history) != 0){
    
    ### PART 1/2: First 100 days of the outbreak, NB: COMEBACK to allow variation in time of detection
    sol <- sol_log %>%
      filter(time <= min(vaccination_history$time)) %>%
      mutate(supply = NA)
     #NB: ncol(sol) == 101 == 20 * (S + E + I + R + incidence) + phase
    
    
    ### PART 2/2: Run with daily time steps when vaccine being delivered
    time_sequence <- seq(max(sol$time)+1, time_horizon, by = 1)

    for (this_phase in unique(vaccination_history$phase)){   
      for (this_supply in unique(vaccination_history$supply[is.na(vaccination_history$supply)==FALSE])){
        for (this_time in time_sequence){
          
          if (this_phase == "essential workers" & this_time >= min(vaccination_history$time[vaccination_history$phase != "essential workers"])){
            
            # skip if after non-exclusive essential worker delivery period
            
          } else if (this_phase == "essential workers" & this_supply != unique(vaccination_history$supply[is.na(vaccination_history$supply)==FALSE])[1]){
            
            # only run delivery to essential workers once (for first "supply" scenario)
            
          } else if(this_phase != "essential workers" & this_time < min(vaccination_history$time[vaccination_history$phase != "essential workers"])){
            
            # skip if delivery to others but in essential worker delivery period
            
          } else {
            
            # start
            if (this_time == min(vaccination_history$time[vaccination_history$phase != "essential workers"])){
              sol <- sol_delivery_to_essential_workers #saved sol because only running once
            }
            
            #reconstruct 'tidy' state
            state_working = tail.matrix(sol, 1)
            state_working = select(state_working, -time, -phase, - supply) #remove all other columns
            state_working = t(state_working)
            colnames(state_working) <- c("individuals")
            
            prev_state = cbind(skeleton_state,state_working)
            if(abs(sum(prev_state$individuals[prev_state$class != "Incid"]) - sum(this_inital_state$individuals))>1){stop(paste("prev state at time step",this_time,"not equal to inital population size!"))}
            
            todays_vaccinations <- vaccination_history %>%
              filter(time == this_time &
                       phase %in% c(this_phase,"essential workers") & #include essential workers always to capture day of concurrent delivery with others
                       (supply == this_supply | is.na(supply))) %>%   # is.na(supply) = NA when essential_workers
              select(-time,-phase) %>%
              group_by(age_group,comorbidity) %>% #sum doses given to essential workers and general population
              summarise(doses_delivered = sum(doses_delivered), .groups = "keep")
            
            if (nrow(todays_vaccinations)>0){
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
                #distribute doses among SEIR classes
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
              
            } else{
              
              next_state <- c(as.numeric(prev_state$individuals[prev_state$class != "Incid"]),
                              rep(0,nrow(prev_state[prev_state$class == "S",]))) #blank Incidence tracker
              
            }
            
            
            #next time step!
            sol <- as.data.frame(ode(y=next_state,
                                     times=(seq(0,1,by=1)),
                                     func=this_configure_ODEs,
                                     parms=this_parameters))
            
            #collect
            sol[,1] <- sol[,1]+1*(this_time-1) #make times correct
            sol <- sol[2,] #remove interim next_state with 0 rows for incidence
            sol <- sol %>% mutate(phase = this_phase,
                                  supply = this_supply)
            sol_log <- rbind(sol_log,sol)
            
            #only run essential workers the first time (nb: run multiple times depending on vax strategies being tested)
            if (this_time == min(vaccination_history$time[vaccination_history$phase != "essential workers"])-1){
              sol_delivery_to_essential_workers <- sol
            }
            
          }
        }
      }
    }
  }

  
  ### TRANSLATE sol of ODEs into incidence_log
  sol_log <- sol_log %>%
    relocate(c("phase","supply"), .after = "time")
  
  #select incidence columns only
  sol_log <- sol_log[,c(1,2,3, #(time,phase,supply)
                        (nrow(this_inital_state[this_inital_state$class == "S",])*4 +3+1):ncol(sol_log))]

  # make tidy
  key = crossing(comorbidity = c(0,1),
                 vaccination_status = c(0,1),
                 age_group = this_age_group_labels) %>%
    mutate(dummy = row_number())
  
  sol_log <- sol_log %>%
    pivot_longer(cols = c(4:ncol(sol_log)),
                 values_to = "incidence",
                 names_to = "dummy") %>%
    mutate(dummy = as.numeric(dummy) - nrow(this_inital_state[this_inital_state$class == "S",])*4) %>%
    left_join(key,by="dummy") %>%
    select(-dummy) %>%
    relocate(incidence, .after = last_col())
  
  #correct to daily Incidence when "no vaccine"
  sol_log = sol_log %>%
    group_by(phase,supply,comorbidity,vaccination_status,age_group) %>%
    mutate(incidence = case_when(
      phase == "no vaccine" ~ incidence - lag(incidence),
      TRUE ~ incidence
    ))
  #NB: 1774560 bytes
  
  #all share first 100 days and essential workers
  workshop = sol_log %>%
    filter(phase %in% c("no vaccine","essential workers")) %>%
    ungroup() %>%
    select(-supply) %>%
    crossing(supply = unique(sol_log$supply[is.na(sol_log$supply) == FALSE])) %>%
    filter(!(supply == 0 & phase != "no vaccine"))
  sol_log = sol_log %>%
    filter(!phase %in% c("no vaccine","essential workers"))
  sol_log = rbind(sol_log,workshop)
  
  #remove time 0 (incidence = NA)
  sol_log <- sol_log %>%
    filter(time>0)

  
  return(sol_log)
}