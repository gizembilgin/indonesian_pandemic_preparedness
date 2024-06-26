

access_simulations <- function(
    simulations_source = "generate", #options: "load", "memory", "generate"
    this_configuration,
    outcome = "infections",
    TOGGLES_project_deaths = list()
){
  
  
  ### Load simulations (optional for each run)
  if (simulations_source == "load"){
    path_stem <- paste0(gsub("/04_shiny","",getwd()),"/04_shiny/x_results/")
    list_poss_Rdata = list.files(
      path = path_stem,
      pattern = "ship_log*"
    )
    if (length(list_poss_Rdata) > 0) {
      list_poss_Rdata_details = double()
      for (j in 1:length(list_poss_Rdata)) {
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste0(path_stem, list_poss_Rdata[[j]]))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste0(path_stem,latest_file))
      load(file = paste0(path_stem,gsub("ship_log","ship_log_key",latest_file))) #load accompanying key
      load(file = paste0(path_stem,gsub("ship_log","indicator_log",latest_file))) #load accompanying indicator_log
    } else{
      stop(paste0("access_simulations: can't find underlying simulation to load! Searching:", path_stem))
    }
    
    ### Subset ship_log to this_configuration
    #NB: leaving supply here to allow cascade to be reconstructed
    #find relevant run_IDs in key
    this_ship_log_key <-  filter_scenarios(ship_log_key,this_configuration[! names(this_configuration) %in% c("supply","phase")])
    #subset ship_log to run_IDs and then phases as well
    this_ship_log <- ship_log %>%
      filter(run_ID %in% this_ship_log_key$run_ID) %>%
      left_join(this_ship_log_key, by = "run_ID") %>%
      select(-run_ID)
    this_ship_log <- filter_scenarios(this_ship_log,this_configuration[names(this_configuration) %in% c("phase")] )
    #also subset indicator_log to this_configuration
    this_indicator_log <- indicator_log %>% rename(phase = strategy)
    this_indicator_log <- filter_scenarios(this_indicator_log,this_configuration[! names(this_configuration) %in% c("supply","R0","vaccine_derived_immunity","infection_derived_immunity","days_to_detection")])
    
    rm(ship_log)
  } else if (simulations_source == "generate"){
    result <- generate_simulations(this_configuration)
    this_ship_log = result$ship_log %>%
      left_join(result$ship_log_key, by = "run_ID") %>%
      select(-run_ID)
    this_indicator_log = result$indicator_log
  }
  
  
  

  ### Reconstruct complete incidence from cascade of simulation
  this_ship_log <- this_ship_log %>%
    mutate(flag_reconstructed = 0) #NB: include as flag so option to not include in incidence plot
  
  additional_rows = data.frame()
  for (this_vaccine_delivery_start_date in unique(this_ship_log$vaccine_delivery_start_date)){
    for (this_rollout_modifier in unique(this_ship_log$rollout_modifier)){
      
      this_workshop <- this_ship_log %>% 
        filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date &
                 rollout_modifier == this_rollout_modifier &
                 ! phase %in% c("no vaccine","healthcare workers"))
      
      #Part 1/2: no vaccine and healthcare worker phases
      this_before_strategy = this_ship_log %>% 
        filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date &
                 rollout_modifier == this_rollout_modifier) %>%
        filter((phase == "no vaccine" & time < vaccine_delivery_start_date) |
                 phase %in% c("healthcare workers")) %>% #CHECKED: unique(to_propogate$supply) == 0 for no vax and 0.2 for healthcare workers
        ungroup() %>%
        select(-phase,-supply)
      before_strategy_contribution = this_workshop %>%
        ungroup() %>%
        select(phase,supply) %>%
        unique() %>%
        crossing(this_before_strategy) %>%
        mutate(flag_reconstructed = 1)
      additional_rows = rbind(additional_rows,before_strategy_contribution)
      
      #Part 2/2: strategy specific phases - cascade!
      supply_loop = unique(this_workshop$supply)
      supply_loop = supply_loop[supply_loop>0]
      supply_loop = as.numeric(sort(supply_loop))
      
      if (length(supply_loop)>1){
        for (this_supply in supply_loop[supply_loop != max(supply_loop)]){
          
          next_supply = supply_loop[which(supply_loop == this_supply)+1]
          next_supply_times = this_workshop %>% 
            filter(supply == next_supply) 
          check = next_supply_times %>% 
            group_by(time,phase) %>% 
            summarise(n=n(), .groups = "keep") %>%
            ungroup() %>%
            select(n) %>%
            unique()
          if (nrow(check)>1){stop("access_simulations: next_supply_times has unequal entries across phases")}
          next_supply_times = next_supply_times %>%
            ungroup() %>%
            select(time) %>%
            unique()
          
          cascade_contribution <- this_workshop %>%
            filter(supply == this_supply &
                     phase != "healthcare workers" &
                     !(time %in% next_supply_times$time)) %>%
            ungroup() %>%
            select(-supply) %>%
            crossing(supply = supply_loop[supply_loop > this_supply])  %>%
            mutate(flag_reconstructed = 1)
          
          additional_rows = rbind(additional_rows,cascade_contribution)
          
        }
      }
    }
  }
  
  #Combine and check
  this_ship_log_completed = rbind(this_ship_log, additional_rows) %>%
    select(-flag_reconstructed) #not currently used
  
  #Check indicator_log that all supplies exist
  workshop <- this_indicator_log %>%
    filter(indicator_speed_sufficient == FALSE)
  if (length(unique(workshop$supply))>1){
    
    for (this_supply in unique(workshop$supply[workshop$supply != min(workshop$supply)])){
      loop_level_1 <- workshop %>% filter(supply == this_supply)
      
      for (this_phase in unique(loop_level_1$phase)){
        loop_level_2 <- loop_level_1 %>% filter(phase == this_phase)
        
        for (this_vaccine_delivery_start_date in unique(loop_level_2$vaccine_delivery_start_date)){
          loop_level_3 <- loop_level_2 %>% filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date)
          
          for (this_rollout_modifier in unique(loop_level_3$rollout_modifier)){
            replacement_rows <- this_ship_log_completed %>%
              filter(rollout_modifier == this_rollout_modifier &
                       vaccine_delivery_start_date == this_vaccine_delivery_start_date &
                       phase == this_phase & 
                       supply == min(workshop$supply)) %>%
              mutate(supply = this_supply)
            this_ship_log_completed <- this_ship_log_completed %>%
              filter(! (rollout_modifier == this_rollout_modifier &
                          vaccine_delivery_start_date == this_vaccine_delivery_start_date &
                          phase == this_phase & 
                          supply == this_supply))
            this_ship_log_completed = rbind(this_ship_log_completed, replacement_rows)
          }

          
        }
      }
    }
  }
  
  check = this_ship_log_completed %>%
    filter(! phase %in% c("healthcare workers", "no vaccine")) %>% 
    group_by(phase,supply,comorbidity,vaccination_status,age_group,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>% 
    summarise(n=n(), .groups = "keep") %>%
    filter(n != max(this_ship_log_completed$time))
  if (nrow(check)>1){stop("access_simulations: not all phase-supply-etc. scenarios have 365 days in this_ship_log_completed")}
  rm(this_ship_log,additional_rows,this_before_strategy,before_strategy_contribution, this_workshop)
  
  if ("supply" %in% names(this_configuration)){#NB: couldn't remove earlier as needed to reconstruct the cascade
    
    this_ship_log_completed <- this_ship_log_completed %>%
      filter(supply %in% this_configuration$supply |  phase %in% c("no vaccine", "healthcare workers"))

  }

  if (outcome == "infections"){
    this_ship_log_completed <- this_ship_log_completed %>%
      group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      summarise(incidence = sum(incidence), .groups = "keep") 
    
  } else if (outcome == "deaths"){
    
    if (length(TOGGLES_project_deaths) == 0 ) stop("access_simulations: you have selected to output deaths but not specified TOGGLES_project_deaths")
    
    this_ship_log_completed <- project_deaths( 
      data = this_ship_log_completed,
      TOGGLES_project_deaths
      ) %>%
      group_by(pathogen,time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      summarise(incidence = sum(deaths), .groups = "keep") 
    
  } else if (outcome == "presentations"){
    
    load(file = paste0(gsub("/04_shiny","","01_inputs/presentations_to_care.Rdata")))
    
    this_ship_log_completed <- this_ship_log_completed %>%
      group_by(time,phase,age_group,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      summarise(incidence = sum(incidence), .groups = "keep") %>%
      left_join(presentations_to_care, by = "age_group") %>%
      mutate(incidence = incidence * proportion) %>%
      group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      summarise(incidence = sum(incidence), .groups = "keep")
    rm(presentations_to_care)
    
  } else{
    stop("access_simulations: you have not specified how to outcome this outcome")
  }
  
  return(this_ship_log_completed)
  
}