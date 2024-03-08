

access_simulations <- function(
    load_simulations = TRUE, #load ship_log every run of accessing the simulation ~ 3 seconds
    this_configuration,
    output = "incidence",
    TOGGLES_project_severe_disease = list()
){
  
  ### Load simulations (optional for each run)
  if (load_simulations == TRUE){
    list_poss_Rdata = list.files(
      path = "04_shiny/x_results/",
      pattern = "ship_log*"
    )
    if (length(list_poss_Rdata) > 0) {
      list_poss_Rdata_details = double()
      for (j in 1:length(list_poss_Rdata)) {
        list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                        file.info(paste0("04_shiny/x_results/", list_poss_Rdata[[j]]))$mtime)
      }
      latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
      load(file = paste0("04_shiny/x_results/",latest_file))
    } else{
      stop("load_smulations: can't find underlying simulation to load!")
    }
  }
  
  
  
  ### Subset ship_log to this_configuration
  this_ship_log <-  filter_scenarios(ship_log,this_configuration[! names(this_configuration) == "supply"])
  if (load_simulations == TRUE) rm(ship_log)
  
  
  
  ### Reconstruct complete incidence from cascade of simulation
  this_ship_log <- this_ship_log %>%
    mutate(flag_reconstructed = 0) #NB: include as flag so option to not include in incidence plot
  
  additional_rows = data.frame()
  for (this_vaccine_delivery_start_date in unique(this_ship_log$vaccine_delivery_start_date)){
    for (this_rollout_modifier in unique(this_ship_log$rollout_modifier)){
      
      this_workshop <- this_ship_log %>% 
        filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date &
                 rollout_modifier == this_rollout_modifier &
                 ! phase %in% c("no vaccine","essential workers"))
      
      #Part 1/2: no vaccine and essential worker phases
      this_before_strategy = this_ship_log %>% 
        filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date &
                 rollout_modifier == this_rollout_modifier) %>%
        filter((phase == "no vaccine" & time < vaccine_delivery_start_date) |
                 phase %in% c("essential workers")) %>% #CHECKED: unique(to_propogate$supply) == 0 for no vax and 0.2 for essential workers
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
        if (nrow(check)>1){stop("fleet_admiral: next_supply_times has unequal entries across phases")}
        next_supply_times = next_supply_times %>%
          ungroup() %>%
          select(time) %>%
          unique()
        
        cascade_contribution <- this_workshop %>%
          filter(supply == this_supply &
                   phase != "essential workers" &
                   !(time %in% next_supply_times$time)) %>%
          ungroup() %>%
          select(-supply) %>%
          crossing(supply = supply_loop[supply_loop > this_supply])  %>%
          mutate(flag_reconstructed = 1)
        
        additional_rows = rbind(additional_rows,cascade_contribution)
        
      }
    }
  }
  
  #Combine and check
  this_ship_log_completed = rbind(this_ship_log, additional_rows) %>%
    select(-flag_reconstructed) #not currently used
  
  check = this_ship_log_completed %>%
    filter(! phase %in% c("essential workers", "no vaccine")) %>% 
    group_by(phase,supply,comorbidity,vaccination_status,age_group,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>% 
    summarise(n=n(), .groups = "keep") %>%
    filter(n != TOGGLE_time_horizon)
  if (nrow(check)>1){stop("fleet_admiral: not all phase-supply-etc. scenarios have 365 days in this_ship_log_completed")}
  rm(this_ship_log,cascade_contribution,additional_rows,this_before_strategy,before_strategy_contribution, this_workshop)
  
  if ("supply" %in% names(this_configuration)){
    #couldn't remove earlier as needed to reconstruct the cascade
    this_ship_log_completed <- this_ship_log_completed %>%
      filter(supply %in% this_configuration$supply |  phase %in% c("no vaccine", "essential workers"))
  }

  if (! output %in% c("cases","deaths")) stop("access_simulations: you have not specified how to output this output")
  if (output == "cases"){
    
    this_ship_log_completed <- this_ship_log_completed %>%
      group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      summarise(incidence = sum(incidence), .groups = "keep") 
    
  } else if (output == "deaths"){
    
    if (length(TOGGLES_project_severe_disease) == 0 ) stop("access_simulations: you have selected to output deaths but not specified TOGGLE_project_severe_disease")
    
    this_ship_log_completed <- project_severe_disease( 
      data = this_ship_log_completed,
      TOGGLES_project_severe_disease
      ) %>%
      group_by(pathogen,time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      summarise(incidence = sum(deaths), .groups = "keep") 
    
  }
  
  return(this_ship_log_completed)
  
}