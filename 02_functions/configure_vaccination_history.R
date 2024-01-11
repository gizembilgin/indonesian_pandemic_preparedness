
# This function creates combinations of vaccination strategies. It starts with the delivery of vaccines to essential workers, before
# providing multiple possible allocation pathways, identified by the variable "phase". 

configure_vaccination_history <- function(LIST_vaccination_strategies = list(),
                                          
                                          vaccine_acceptance = loaded_setting_characteristics$vaccine_acceptance, 
                                          daily_vaccine_delivery_capacity = loaded_setting_characteristics$daily_vaccine_delivery_capacity,
                                          population_by_comorbidity = loaded_setting_characteristics$population_by_comorbidity,
                                          essential_workers = loaded_setting_characteristics$essential_workers){
  
  
  
  ### RETURN if no vaccination strategy specified
  if (length(LIST_vaccination_strategies) == 0) return(vaccination_history = data.frame())
  
  
  
  ### CHECK that LIST_vaccination_strategies provided as expected
  #NB: checking that a label has been provided for each vaccination strategy, and that age groups and comorbidity statuses provided are valid values
  check_configured_strategies <- LIST_vaccination_strategies$strategy
  for(this_strategy in check_configured_strategies){
    if(grepl('[0-9]', substr(this_strategy[[1]],1,1))){stop("the label of your vaccination strategies should start with character descriptions NOT digits")}
    this_strategy <- this_strategy[-1]
    
    if (is.list(this_strategy) == FALSE) stop("vaccination strategies not provided as a list")
    for (this_stage in this_strategy){
      if (is.list(this_stage) == FALSE) stop("not all vaccination strategies entered as lists")
        if (length(this_stage) != 2) stop("you need to specify the comorbidities and age group of each prioritisation stage")
        if (sum(!this_stage[[1]] %in% age_group_labels) != 0) stop("you appear to have mistyped an age_group when defining your vaccination strategies")
        if (sum(!this_stage[[2]] %in% c(0, 1)) != 0) stop("you appear to have mistyped comorbidity status when defining your vaccination strategies")
    }
  }
  rm(this_strategy,this_stage)
  

  
  ### DELIVER VACCINES TO ESSENTIAL WORKERS
  this_vaccine_acceptance <- vaccine_acceptance[vaccine_acceptance$phase == "essential workers",-c(1)]

  essential_worker_target <- population_by_comorbidity %>%
    left_join(essential_workers, by = "age_group") %>%
    left_join(this_vaccine_acceptance, by = "comorbidity") %>%
    mutate(individuals = individuals * proportion * uptake) %>%
    select(-uptake) %>%
    mutate(proportion = individuals/sum(individuals)) %>% #calculate this age/comorb combination as a proportion of all essential workers
    mutate(doses_delivered = proportion * daily_vaccine_delivery_capacity) #calculate doses_delivered on a day of full capacity delivery to essential workers
  if(round(sum(essential_worker_target$proportion),digits=2) != 1){stop("proportion calculation on essential_worker_target not EQ 100%")}
  
  essential_worker_timeframe = floor(sum(essential_worker_target$individuals)/daily_vaccine_delivery_capacity) #create sequence of days for full capacity delivery to essential workers
  time_sequence = seq(LIST_vaccination_strategies$vaccine_delivery_start_date,LIST_vaccination_strategies$vaccine_delivery_start_date+essential_worker_timeframe-1,by=1)
  if(length(time_sequence) != essential_worker_timeframe){stop("time sequence not aligning with timeframe for delivery of doses to essential workers")}
  
  essential_worker_delivery = crossing(time = time_sequence,essential_worker_target) 
  final_row_delivery =  sum(essential_worker_target$individuals) - sum(essential_worker_delivery$doses_delivered)
  final_row = essential_worker_target %>%
    mutate(doses_delivered = proportion * final_row_delivery,
           time = LIST_vaccination_strategies$vaccine_delivery_start_date + essential_worker_timeframe)
  essential_worker_delivery <- rbind(essential_worker_delivery,final_row) %>%
    select(-proportion,-individuals) %>%
    mutate(phase = "essential workers")
  if(abs(sum(essential_worker_delivery$doses_delivered) - sum(essential_worker_target$individuals))>1) stop("doses delivered to essential workers does not align with essential worker target")
  if(nrow(essential_worker_delivery[essential_worker_delivery$doses_delivered<0,])>0) stop("negative doses delivered to essential workers")
  
  
  
  ### DELIVER VACCINES AS PER ALLOCATION STRATEGIES LISTED
  this_vaccine_acceptance <- vaccine_acceptance[vaccine_acceptance$phase != "essential workers",-c(1)]
  
  remainder_of_population_target <- population_by_comorbidity %>%
    left_join(essential_workers, by = "age_group") %>%
    left_join(this_vaccine_acceptance, by = "comorbidity") %>%
    mutate(individuals = individuals * (1-proportion) * uptake) %>%
    select(-proportion,-uptake)
  
  partial_dose_delivery_on_first_day = daily_vaccine_delivery_capacity - final_row_delivery
  if (partial_dose_delivery_on_first_day<0){stop("first day of delivery for people who aren't essential workers is negative")}
  
  target_population_delivery = data.frame()
  for (this_supply in LIST_vaccination_strategies$supply){
    for (this_strategy_index in 1:length(LIST_vaccination_strategies$strategy)){
      
      this_phase = as.character(LIST_vaccination_strategies$strategy[[this_strategy_index]][1])
      this_strategy = LIST_vaccination_strategies$strategy[[this_strategy_index]][-1]
      this_population_target <- remainder_of_population_target
      this_population_target$priority <- NA
      
      #CHECK value of this_supply, then convert from proportion to absolute number
      if(this_supply > 1 | this_supply < 0) stop("vaccination supply not within 0 to 1")
      if (this_supply < sum(essential_worker_target$individuals)/sum(population_by_comorbidity$individuals)){
        stop(paste0("This supply (",this_supply*100,"%) is not sufficient to reach essential workers. Please specify a supply of at least ",
                    round(sum(essential_worker_target$individuals)/sum(population_by_comorbidity$individuals) * 100,digits=0), "%.",
                    " There is no point comparing vaccine prioritisation decisions without prioritisation decisions."))
      } 
      this_supply_abs = this_supply * sum(population_by_comorbidity$individuals) - sum(essential_worker_target$individuals)
      
      #ASSIGN priority number
      for (priority_num in 1:length(this_strategy)){
        this_population_target$priority[ this_population_target$age_group %in% unlist(this_strategy[priority_num]) &
                                           this_population_target$comorbidity %in% unlist(this_strategy[priority_num])] <- priority_num
      }
      
      #ALIGN this_population_target with supply
      this_population_target <- this_population_target %>%
        filter(is.na(priority) == FALSE) 
      if (this_supply_abs > sum(this_population_target$individuals)) this_supply_abs = sum(this_population_target$individuals)
      
      while (round(sum(this_population_target$individuals)) > round(this_supply_abs)){
        #if df without last priority still > supply, remove last priority group
        if (sum(this_population_target$individuals[this_population_target$priority < max(this_population_target$priority)]) > this_supply_abs){
          this_population_target <- this_population_target %>% filter(priority != max(this_population_target$priority))
        
        } else{ #removing some from the last priority group to align supply with delivery target
          isolate_last_priority <-  this_population_target %>% filter(priority == max(this_population_target$priority))
          multiplier <- 1 - (sum(this_population_target$individuals) - this_supply_abs)/sum(isolate_last_priority$individuals)
          
          this_population_target <- this_population_target %>%
            mutate(individuals = case_when(
              priority == unique(isolate_last_priority$priority) ~ individuals * multiplier,
              TRUE ~ individuals
            ))
          rm(isolate_last_priority, multiplier)
        }
      }
      if (round(sum(this_population_target$individuals)) != round(this_supply_abs)) stop("not as many vaccines delivered as possible")
      
      this_population_target <- this_population_target %>%
        mutate(proportion = individuals/sum(individuals)) %>% #calculate this age/comorb combination as a proportion of all target population
        mutate(doses_delivered = proportion * daily_vaccine_delivery_capacity) #calculate doses_delivered on a day of full capacity delivery to all target population
      if(round(sum(this_population_target$proportion),digits=2) != 1) stop("proportion calculation on this_population_target not EQ 100%")
      
      # first day
      first_day_target_population <- this_population_target %>%
        mutate(doses_delivered = partial_dose_delivery_on_first_day * proportion,
               time = max(essential_worker_delivery$time))
      
      # full delivery days
      population_target_timeframe = floor((sum(this_population_target$individuals)-partial_dose_delivery_on_first_day)/daily_vaccine_delivery_capacity) 
      time_sequence = seq(max(essential_worker_delivery$time)+1,max(essential_worker_delivery$time)+population_target_timeframe,by=1)
      if(length(time_sequence) != population_target_timeframe){stop("time sequence not aligning with timeframe for delivery of doses to the target population")}
      this_target_population_delivery = crossing(time = time_sequence,this_population_target) 
      
      # last day
      final_row_delivery =  sum(this_population_target$individuals) - sum(this_target_population_delivery$doses_delivered) - sum(first_day_target_population$doses_delivered)
      final_row = this_population_target %>%
        mutate(doses_delivered = proportion * final_row_delivery,
               time = max(this_target_population_delivery$time) + 1)
      
      # bring together
      this_target_population_delivery <- rbind(first_day_target_population,this_target_population_delivery,final_row) %>%
        select(-proportion,-individuals) %>%
        mutate(phase = this_phase,
               supply = this_supply) %>%
        select(-priority)
      if(round(sum(this_target_population_delivery$doses_delivered)) != round(sum(this_population_target$individuals))){stop("delivery to target population not aligning with target population")}
      if(nrow(this_target_population_delivery[this_target_population_delivery$doses_delivered<0,])>0){stop("negative doses delivered to essential workers")}
  
      #CHECK: never > daily_capacity OR supply
      check <- bind_rows(essential_worker_delivery,this_target_population_delivery) %>%
        group_by(time) %>%
        summarise(total_daily_delivered = sum(doses_delivered)) %>%
        filter(round(total_daily_delivered) > daily_vaccine_delivery_capacity)
      if (nrow(check)>0){stop(paste("daily capacity of vaccine delivery exceeded at time step",check$time[1]))}
      
      #CHECK: delivery did not exceed supply
      check <- bind_rows(essential_worker_delivery,this_target_population_delivery) %>%
        summarise(total_daily_delivered = sum(doses_delivered)) %>%
        filter(total_daily_delivered > this_supply * sum(population_by_comorbidity$individuals)) %>%
        filter(abs(total_daily_delivered - (this_supply * sum(population_by_comorbidity$individuals)))>1)
      if (nrow(check)>0) stop("vaccine delivery exceeded supply")

      target_population_delivery = rbind(target_population_delivery,this_target_population_delivery)
      
    }
  }
  
  vaccination_history_permutations <- bind_rows(essential_worker_delivery,target_population_delivery) %>%
    filter(doses_delivered >0)

  #CHECK never deliver more doses than individuals
  for (this_phase in unique(vaccination_history_permutations$phase)){
    for (this_supply in unique(vaccination_history_permutations$supply)){
      
      check <- vaccination_history_permutations %>% 
        filter(phase %in% c(this_phase,"essential workers") &
                 (supply == this_supply | is.na(supply))) %>%
        group_by(age_group,comorbidity) %>%
        summarise(doses_delivered = sum(doses_delivered), .groups = "keep") %>%
        left_join(population_by_comorbidity, by = c("age_group","comorbidity")) %>%
        filter(individuals < doses_delivered)
      
      if (nrow(check)>0){stop(paste("too many doses delivered to some combination of", 
                                    unique(check$age_group),
                                    "with",
                                    unique(check$comorbidity)))}
    }
  }
  
  #CHECK no gap day
  if (length(unique(vaccination_history_permutations$time)) !=
      length(seq(min(vaccination_history_permutations$time),max(vaccination_history_permutations$time)))){
    stop("there is a time gap in the vaccination_history!")
  }
  
  
    
  return (vaccination_history_permutations)
}