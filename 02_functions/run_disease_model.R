
run_disease_model <- function(time_horizon = 365,
                              vaccination_history = data.frame(),
                              
                              this_inital_state = inital_state,
                              this_configure_ODEs = configure_ODEs,
                              this_parameters = parameters,
                              this_age_group_labels = age_group_labels) {
  
  
  state = c(this_inital_state$individuals) 
  skeleton_state <- this_inital_state %>% select(-individuals)
  
  
  # run for entire time_horizon without vaccination (baseline)
  sol_log = as.data.frame(ode(y=state,
                          times=seq(0,time_horizon,by=1),
                          func=this_configure_ODEs,
                          parms=this_parameters)) %>%
    mutate(phase = "no vaccine",
           supply = 0,
           cumulative_flag = 1)
  #NB: ncol(sol) == 104 == time + 20 * (S + E + I + R + incidence) + phase + supply + cumulative flag = 1 + 100 + 3  
  
if (nrow(vaccination_history) != 0){

    # run with daily time steps when vaccine being delivered
    time_sequence <- seq(min(vaccination_history$time), time_horizon, by = 1)

    for (this_phase in unique(vaccination_history$phase)){   
      for (this_supply in unique(vaccination_history$supply[is.na(vaccination_history$supply)==FALSE])){
        
        this_vaccination_history <- vaccination_history %>%
          filter(phase == this_phase &
                   (supply == this_supply | is.na(supply)))
        this_time_sequence = time_sequence[time_sequence %in% unique(this_vaccination_history$time)]
        rm(this_vaccination_history)
        
        # load cascade point (see: 99_scanned schematics/2023_01_15 cascade of simulations.pdf)
        if (this_phase == "essential workers"){
          sol <- sol_log %>%
            filter(time == min(this_time_sequence) -1) %>%
            mutate(supply = NA)
        } else if (this_supply == unique(vaccination_history$supply[is.na(vaccination_history$supply)==FALSE])[1]){
          sol <- sol_handover_essential_workers
        } else {
          sol <- sol_handover                 
        }

        if(this_supply != unique(vaccination_history$supply[is.na(vaccination_history$supply)==FALSE])[1]){
          this_time_sequence <- this_time_sequence[this_time_sequence > sol$time]
        }
        
        for (this_time in this_time_sequence){
          
          if (this_phase == "essential workers" & this_time >= min(vaccination_history$time[vaccination_history$phase != "essential workers"])){
            # skip if after non-exclusive essential worker delivery period, ensures that day of overlap not missed!
          } else if (this_phase == "essential workers" & this_supply != unique(vaccination_history$supply[is.na(vaccination_history$supply)==FALSE])[1]){
            # only run delivery to essential workers once (for first "supply" scenario)
          }  else {
            
            #reconstruct 'tidy' state
            state_working = sol %>%
              filter(time == max(sol$time)) %>%
              select(-time,-phase,-supply,-cumulative_flag)
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
             
            #CHECK: (INTENSIVE) vaccination history aligns with next_state configuration
            # expected_numbers <- vaccination_history %>%
            #   filter(time <= this_time) %>%
            #   filter(phase %in% c(this_phase,"essential workers") & #include essential workers always to capture day of concurrent delivery with others
            #            (supply == this_supply | is.na(supply))) %>%
            #   group_by(age_group,comorbidity) %>%
            #   summarise(doses_delivered = sum(doses_delivered), .groups = "keep")
            # 
            # actual_numbers <- next_state %>% 
            #   filter(vaccination_status == 1 &
            #            individuals != 0) %>%
            #   group_by(age_group,comorbidity) %>%
            #   summarise(individuals = sum(individuals), .groups = "keep")
            # 
            # check <- expected_numbers %>%
            #   left_join(actual_numbers, by= c("age_group","comorbidity")) %>%
            #   filter(round(individuals) != round(doses_delivered))
            # if(nrow(check)>0){stop("vaccination misaligned here")}   
            
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
            sol[,1] <- sol[,1]+(this_time-1) #make times correct
            sol <- sol[2,] #remove interim next_state with 0 rows for incidence
            sol <- sol %>% mutate(phase = this_phase,
                                  supply = this_supply,
                                  cumulative_flag = 0)
            sol_log <- rbind(sol_log,sol)
            
            #only run essential workers the first time (nb: run multiple times depending on vax strategies being tested)
            if (this_time == min(vaccination_history$time[vaccination_history$phase != "essential workers"])-1){
              sol_handover_essential_workers <- sol
            }
            #save and use as start point for next supply in this phase
            if (this_phase != "essential workers" & this_time == max(this_time_sequence)-1){
              sol_handover <- sol
            }
            #NB: for both using -1 max date, because not at full capacity on final day
            
          }
        }
        
        #run remainder where no longer daily vaccine supply        
        if (this_phase != "essential workers"){

          sol <- as.data.frame(ode(y=next_state,
                                   times=seq(this_time-1,time_horizon,by=1),
                                   func=this_configure_ODEs,
                                   parms=this_parameters))
          #remove rows already attached to sol_log
          sol <- sol[-c(1,2),]
          
          #add to solution log
          sol <- sol %>% mutate(phase = this_phase,
                                supply = this_supply,
                                cumulative_flag = 1)
          
          sol_log <- rbind(sol_log,sol)

        }
      }
    }
  }
  
  ### TRANSLATE sol of ODEs into incidence_log
  sol_log <- sol_log %>%
    relocate(c("phase","supply","cumulative_flag"), .after = "time")
  
  #select incidence columns only
  sol_log <- sol_log[,c(1,2,3,4, #(time,phase,supply,cumulative_flag)
                        (nrow(this_inital_state[this_inital_state$class == "S",])*4 + 4 + 1):ncol(sol_log))]

  # make tidy
  key = crossing(comorbidity = c(0,1),
                 vaccination_status = c(0,1),
                 age_group = this_age_group_labels) %>%
    mutate(dummy = row_number())
  
  sol_log <- sol_log %>%
    pivot_longer(cols = c((4+1):ncol(sol_log)),
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
      cumulative_flag == 1 ~ incidence - lag(incidence),
      TRUE ~ incidence
    ))

  #remove time 0 (incidence = NA)
  sol_log <- sol_log %>%
    filter(time>0) %>%
    select(-cumulative_flag)

  return(sol_log)
}