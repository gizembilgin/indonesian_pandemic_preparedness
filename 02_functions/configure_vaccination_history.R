
# This function creates combinations of vaccination strategies. It starts with the delivery of vaccines to healthcare workers, before
# providing multiple possible allocation pathways, identified by the variable "phase". 

configure_vaccination_history <- function(LIST_vaccination_strategies = list(),
                                          
                                          vaccine_acceptance = loaded_setting_characteristics$vaccine_acceptance, 
                                          daily_vaccine_delivery = loaded_setting_characteristics$daily_vaccine_delivery,
                                          population_by_comorbidity = loaded_setting_characteristics$population_by_comorbidity,
                                          healthcare_workers = loaded_setting_characteristics$healthcare_workers,
                                          
                                          daily_vaccine_delivery_realistic = FALSE #if == TRUE then capacity to deliver vaccines slowly builds up over the first few months
                                          ){
  
  
  
  ### RETURN if no vaccination strategy specified
  if (length(LIST_vaccination_strategies) == 0) return(vaccination_history = data.frame())
  

  ### CHECK that LIST_vaccination_strategies provided as expected
  #NB: checking that a label has been provided for each vaccination strategy, and that age groups and comorbidity statuses provided are valid values
  check_configured_strategies <- LIST_vaccination_strategies$strategy
  for(this_strategy in check_configured_strategies){
    if(grepl('[0-9]', substr(this_strategy[[1]],1,1))) stop("the label of your vaccination strategies should start with character descriptions NOT digits")
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
  
  
  ### CONFIGURE daily_vaccine_delivery
  # apply rollout_modifier
  daily_vaccine_delivery$capacity  = daily_vaccine_delivery$capacity  * LIST_vaccination_strategies$rollout_modifier
  
  # filter if you don't want a realistic gradual increase in delivery capacity
  if (daily_vaccine_delivery_realistic == FALSE){
    #select final row, speed of rollout when at full capacity with sufficient supply
    daily_vaccine_delivery <- daily_vaccine_delivery[nrow(daily_vaccine_delivery),]
  }
  
  # build daily_capacity_df
  daily_capacity_df <- data.frame(time = seq(LIST_vaccination_strategies$vaccine_delivery_start_date, time_horizon),
                                         capacity = 0)
  for (this_row in 1:nrow(daily_vaccine_delivery)){
    find_time <- min(daily_capacity_df$time[daily_capacity_df$capacity == 0])
    daily_capacity_df <- daily_capacity_df %>%
      mutate(capacity = case_when(
        capacity == 0 & time <= find_time + daily_vaccine_delivery$days[this_row] - 1 ~ daily_vaccine_delivery$capacity[this_row],
        TRUE ~ capacity
      ))
  }
  # ggplot(daily_capacity_df) + geom_point(aes(time,capacity))
  # daily_capacity_df %>% group_by(capacity) %>% summarise(n = n()) #CHECKED: as expected
  
  # calculate max supply
  max_supply <-sum(daily_capacity_df$capacity)/ sum(population_by_comorbidity$individuals)
  
  # calculate end simulation time for each rollout capacity phase
  find_time <- daily_capacity_df %>% 
    group_by(capacity) %>% 
    summarise(time = max(time)) 
  daily_vaccine_delivery <- daily_vaccine_delivery %>%
    left_join(find_time, by = "capacity")

  

  ### DELIVER VACCINES TO healthcare workerS
  this_vaccine_acceptance <- vaccine_acceptance[vaccine_acceptance$phase == "healthcare workers",-c(1)]

  workshop_healthcare_worker_target <- healthcare_worker_target <- population_by_comorbidity %>%
    left_join(healthcare_workers, by = "age_group") %>%
    left_join(this_vaccine_acceptance, by = "comorbidity") %>%
    mutate(individuals = individuals * proportion * uptake) %>%
    filter(individuals != 0) %>%
    select(-uptake) %>%
    mutate(proportion = individuals/sum(individuals)) %>% #calculate this age/comorb combination as a proportion of all healthcare workers
    mutate(doses_delivered = proportion * daily_vaccine_delivery$capacity[1]) #calculate doses_delivered on a day of full capacity delivery to healthcare workers
  if(round(sum(healthcare_worker_target$proportion),digits=2) != 1) stop("proportion calculation on healthcare_worker_target not EQ 100%")
  
  #configure data set
  min_date = LIST_vaccination_strategies$vaccine_delivery_start_date  
  healthcare_worker_delivery <- data.frame()
  
  #if healthcare workers take up multiple rollout capacity phases
  while(nrow(daily_vaccine_delivery) > 1 && 
    sum(workshop_healthcare_worker_target$individuals) >  daily_vaccine_delivery$capacity[1] *  daily_vaccine_delivery$days[1]){
    these_rows <- crossing(time = seq(min_date, 
                                      min_date + daily_vaccine_delivery$days[1] - 1),
                           workshop_healthcare_worker_target) 
    healthcare_worker_delivery <- rbind(healthcare_worker_delivery,these_rows)
    
    daily_vaccine_delivery <- daily_vaccine_delivery[-c(1),]
    #update daily doses delivered
    workshop_healthcare_worker_target  <- workshop_healthcare_worker_target %>%
      mutate(doses_delivered = proportion * daily_vaccine_delivery$capacity[1]) 
    #update min_date
    min_date = max(healthcare_worker_delivery$time) + 1
    #update individuals remaining to be vaccinated
    these_rows <- these_rows %>%
      group_by(age_group, comorbidity) %>%
      summarise(individuals_vaccinated = sum(doses_delivered), .groups = "keep") %>%
      select(age_group, comorbidity,individuals_vaccinated)
    workshop_healthcare_worker_target <- workshop_healthcare_worker_target %>%
      left_join(these_rows, by = c("age_group","comorbidity")) %>%
      mutate(individuals = individuals - individuals_vaccinated) %>%
      select(-individuals_vaccinated)
  }
  
  healthcare_worker_timeframe = floor(sum(workshop_healthcare_worker_target$individuals)/daily_vaccine_delivery$capacity[1]) #create sequence of days for full capacity delivery to healthcare workers
  time_sequence = seq(min_date,min_date+healthcare_worker_timeframe-1,by=1)
  if(length(time_sequence) != healthcare_worker_timeframe) stop("time sequence not aligning with timeframe for delivery of doses to healthcare workers")
  
  these_rows = crossing(time = time_sequence,workshop_healthcare_worker_target) 
  final_row_delivery =  sum(workshop_healthcare_worker_target$individuals) - sum(these_rows$doses_delivered)
  final_row = workshop_healthcare_worker_target %>%
    mutate(doses_delivered = proportion * final_row_delivery,
           time = min_date + healthcare_worker_timeframe)
  healthcare_worker_delivery <- rbind(healthcare_worker_delivery,these_rows,final_row) %>%
    select(-proportion,-individuals) %>%
    mutate(phase = "healthcare workers")
  if(abs(sum(healthcare_worker_delivery$doses_delivered) - sum(healthcare_worker_target$individuals))>1) stop("doses delivered to healthcare workers does not align with healthcare worker target")
  if(nrow(healthcare_worker_delivery[healthcare_worker_delivery$doses_delivered<0,])>0) stop("negative doses delivered to healthcare workers")
  
  partial_dose_delivery_on_first_day_healthcare_workers = daily_vaccine_delivery$capacity[1] - final_row_delivery
  if (partial_dose_delivery_on_first_day_healthcare_workers<0) stop("first day of delivery for people who aren't healthcare workers is negative")
  
  
  ### DELIVER VACCINES AS PER ALLOCATION STRATEGIES LISTED
  this_vaccine_acceptance <- vaccine_acceptance[vaccine_acceptance$phase != "healthcare workers",-c(1)]
  
  remainder_of_population_target <- population_by_comorbidity %>%
    left_join(healthcare_workers, by = "age_group") %>%
    left_join(this_vaccine_acceptance, by = "comorbidity") %>%
    mutate(individuals = individuals * (1-proportion) * uptake) %>%
    select(-proportion,-uptake)
  

  target_population_delivery = data.frame()
  indicator_delivery_within_time_horizon = data.frame()
  
  for (this_supply in LIST_vaccination_strategies$supply){
    
    #CHECK value of this_supply
    if(this_supply > 1 | this_supply < 0) stop("vaccination supply not within 0 to 1")
    if (this_supply < sum(healthcare_worker_target$individuals)/sum(population_by_comorbidity$individuals)){
      stop(paste0("This supply (",this_supply*100,"%) is not sufficient to reach healthcare workers. Please specify a supply of at least ",
                  round(sum(healthcare_worker_target$individuals)/sum(population_by_comorbidity$individuals) * 100,digits=0), "%.",
                  " There is no point comparing vaccine prioritisation decisions without prioritisation decisions."))
    } 
    
    for (this_strategy_index in 1:length(LIST_vaccination_strategies$strategy)){
      
      this_phase = as.character(LIST_vaccination_strategies$strategy[[this_strategy_index]][1])
      this_strategy = LIST_vaccination_strategies$strategy[[this_strategy_index]][-1]
      this_population_target <- remainder_of_population_target
      this_population_target$priority <- NA
      
      
      #ASSIGN priority number
      for (priority_num in 1:length(this_strategy)){
        this_population_target$priority[ this_population_target$age_group %in% unlist(this_strategy[priority_num]) &
                                           this_population_target$comorbidity %in% unlist(this_strategy[priority_num])] <- priority_num
      }
      this_population_target <- this_population_target %>%
        filter(is.na(priority) == FALSE)
      
      
      #CREATE indicators of if this_supply delivered within simulation
      this_row = data.frame(
        time_horizon = time_horizon,
        vaccine_delivery_start_date = LIST_vaccination_strategies$vaccine_delivery_start_date,
        rollout_modifier = LIST_vaccination_strategies$rollout_modifier,
        supply = this_supply,
        delivered_supply = min(max_supply,this_supply),
        strategy = this_phase)
      
      #indicator_speed_sufficient == speed sufficient for delivery within time horizon
      if (this_supply>max_supply)   this_row$indicator_speed_sufficient = FALSE  else  this_row$indicator_speed_sufficient = TRUE
      this_supply_modified = min(max_supply,this_supply)
      
      #indicator_supply_delivered == size of priority group uses complete supply      
      if (sum(this_population_target$individuals) + sum(healthcare_worker_target$individuals) < this_supply * sum(population_by_comorbidity$individuals)) {
        this_row$indicator_supply_delivered = FALSE
      } else{
        this_row$indicator_supply_delivered = TRUE
      }
      indicator_delivery_within_time_horizon = rbind(indicator_delivery_within_time_horizon,this_row)
      
      
      # CALCULATE absolute number of supply and ALIGN this_population_target with supply
      this_supply_abs = this_supply_modified * sum(population_by_comorbidity$individuals) - sum(healthcare_worker_target$individuals)
      if (this_supply_abs > sum(this_population_target$individuals)) this_supply_abs = sum(this_population_target$individuals)

      
      #CALCULATE doses delivered to target population by age/comorbidity based on supply
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
      
      
      #CALCULATE daily dose delivery by priority group
      this_loop_daily_vaccine_delivery <- daily_vaccine_delivery
      
      this_population_target <- this_population_target %>%
        group_by(priority) %>%
        mutate(proportion = individuals/sum(individuals)) %>% #calculate this age/comorb combination as a proportion of all target population
        mutate(doses_delivered = proportion * this_loop_daily_vaccine_delivery$capacity[1]) %>% #calculate doses_delivered on a day of full capacity delivery to all target population
        filter(doses_delivered != 0)
      if(round(sum(this_population_target$proportion),digits=2) != length(unique(this_population_target$priority))) stop("proportion calculation on this_population_target not EQ 100%")
      
      
      this_vax_history <- healthcare_worker_delivery
      
      
      #ALLOCATE doses by day
      for (this_priority in sort(unique(this_population_target$priority))){
        
        if (this_priority == 1){
          partial_dose_delivery_on_first_day = partial_dose_delivery_on_first_day_healthcare_workers
        }
        
        workshop_priority_target_population <- this_priority_target_population  <- this_population_target %>%
          filter(priority == this_priority)  %>% 
          mutate(doses_delivered = proportion * this_loop_daily_vaccine_delivery$capacity[1])
        this_target_population_delivery = data.frame()
        
        # first day - partial delivery <-> align with previous phase
        first_day_target_population <- workshop_priority_target_population %>%
          mutate(doses_delivered = partial_dose_delivery_on_first_day * proportion,
                 time = max(this_vax_history$time))
        #update to remove first day delivery
        workshop_priority_target_population$individuals <- workshop_priority_target_population$individuals  - workshop_priority_target_population$proportion * partial_dose_delivery_on_first_day
        min_date = max(this_vax_history$time) + 1

        
        #if delivery take up multiple rollout capacity phases
        while(nrow(this_loop_daily_vaccine_delivery) > 1 && 
              sum(workshop_priority_target_population$individuals)  >  
              this_loop_daily_vaccine_delivery$capacity[1] *  (this_loop_daily_vaccine_delivery$time[1] - min_date )){
          
          these_rows <- crossing(time = seq(min_date, 
                                            min_date + this_loop_daily_vaccine_delivery$time[1] - 1),
                                 workshop_priority_target_population) 
          this_target_population_delivery <- rbind(this_target_population_delivery,these_rows)
          
          this_loop_daily_vaccine_delivery <- this_loop_daily_vaccine_delivery[-c(1),]
          #update daily doses delivered
          workshop_priority_target_population  <- workshop_priority_target_population %>%
            mutate(doses_delivered = proportion * this_loop_daily_vaccine_delivery$capacity[1]) 
          #update min_date
          min_date = max(this_target_population_delivery$time) + 1
          #update individuals remaining to be vaccinated
          these_rows <- these_rows %>%
            group_by(age_group, comorbidity) %>%
            summarise(individuals_vaccinated = sum(doses_delivered), .groups = "keep") %>%
            select(age_group, comorbidity,individuals_vaccinated)
          workshop_priority_target_population <- workshop_priority_target_population %>%
            left_join(these_rows, by = c("age_group","comorbidity")) %>%
            mutate(individuals = individuals - individuals_vaccinated) %>%
            select(-individuals_vaccinated)
          
        }
        
        # full delivery days
        priority_target_timeframe = floor(sum(workshop_priority_target_population$individuals)/this_loop_daily_vaccine_delivery$capacity[1]) 
        time_sequence = seq(min_date,min_date+priority_target_timeframe-1,by=1)
        if(length(time_sequence) != priority_target_timeframe ) stop("time sequence not aligning with timeframe for delivery of doses to the target population")
        these_rows = crossing(time = time_sequence,
                              workshop_priority_target_population) 
        
        # last day - partial delivery <-> align with doses left
        final_row_delivery =  sum(workshop_priority_target_population$individuals) - sum(these_rows$doses_delivered)
        final_row = workshop_priority_target_population %>%
          mutate(doses_delivered = proportion * final_row_delivery,
                 time = max(these_rows$time) + 1)
        
        partial_dose_delivery_on_first_day = this_loop_daily_vaccine_delivery$capacity[1] - final_row_delivery
        if (partial_dose_delivery_on_first_day<0) stop("configure_vaccination_history: more doses delivered in final_row_delivery of strategy than capacity")
        
        this_target_population_delivery <- rbind(first_day_target_population,this_target_population_delivery,these_rows,final_row) %>%
          ungroup() %>%
          select(-proportion,-individuals,-priority) %>%
          mutate(phase = this_phase,
                 supply = this_supply)
        
        this_vax_history <- bind_rows(this_vax_history,this_target_population_delivery)
        
      }
      
      
      #CHECK: never > daily_capacity OR supply
      check <- this_vax_history %>%
        group_by(time) %>%
        summarise(total_daily_delivered = sum(doses_delivered)) %>%
        left_join(daily_capacity_df, by = "time") %>%
        filter(round(total_daily_delivered-capacity)>0)
      if (nrow(check)>0) stop(paste("daily capacity of vaccine delivery exceeded at time step",check$time[1]))
      
      
      #CHECK: delivery did not exceed supply
      check <- this_vax_history %>%
        summarise(total_daily_delivered = sum(doses_delivered)) %>%
        filter(total_daily_delivered > this_supply * sum(population_by_comorbidity$individuals)) %>%
        filter(abs(total_daily_delivered - (this_supply * sum(population_by_comorbidity$individuals)))>1)
      if (nrow(check)>0) stop("vaccine delivery exceeded supply")

      this_target_population_delivery <- this_vax_history %>%
        filter(phase != "healthcare workers")
      
      
      #CHECK: delivery to target population
      if(round(sum(this_target_population_delivery$doses_delivered)) != round(sum(this_population_target$individuals))) stop("delivery to target population not aligning with target population")
      if(nrow(this_target_population_delivery[round(this_target_population_delivery$doses_delivered,digits=2)<0,])>0)   stop("negative doses delivered to target population")
      
      target_population_delivery = rbind(target_population_delivery,this_target_population_delivery)
      
    }
  }
  
  vaccination_history_permutations <- bind_rows(healthcare_worker_delivery,target_population_delivery)
  
  #CHECK: never deliver more doses than individuals
  for (this_phase in unique(vaccination_history_permutations$phase)){
    for (this_supply in unique(vaccination_history_permutations$supply)){
      
      check <- vaccination_history_permutations %>% 
        filter(phase %in% c(this_phase,"healthcare workers") &
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
  
  #CHECK: no gap day
  if (length(unique(vaccination_history_permutations$time)) !=
      length(seq(min(vaccination_history_permutations$time),max(vaccination_history_permutations$time)))){
    stop("there is a time gap in the vaccination_history!")
  }
  
  #CHECK: all supply levels have same middle-period delivery (to ensure cascade in run_disease_model)
  for (this_phase in unique(vaccination_history_permutations$phase[vaccination_history_permutations$phase != "healthcare workers"])){
    check = vaccination_history_permutations %>% 
      filter(phase %in% c(this_phase)) %>% 
      mutate(doses_delivered = round(doses_delivered,digits = 2)) %>%
      group_by(time,age_group,comorbidity) %>%
      reframe(n = unique(doses_delivered)) %>%
      group_by(time,age_group,comorbidity) %>%
      reframe(n = n()) %>%
      filter(n>1) %>%
      group_by(time) %>%
      reframe(n = unique(n))
    if (nrow(check) > length(LIST_vaccination_strategies$supply) - 1) stop("cascade in run_disease_model will NOT work for this configured vaccination history")
  }

  result = list(indicator_delivery_within_time_horizon = indicator_delivery_within_time_horizon,
                vaccination_history = vaccination_history_permutations)  
  return (result)
}