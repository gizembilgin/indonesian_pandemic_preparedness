
configure_vaccination_history <- function(TOGGLE_vaccination_strategy = list(),
                                          vaccine_acceptance = load_setting_characteristics$vaccine_acceptance, 
                                          vaccine_delivery_capacity = load_setting_characteristics$vaccine_delivery_capacity,
                                          population_by_risk_group = loaded_setting_characteristics$population_by_risk_group,
                                          essential_workers = loaded_setting_characteristics$essential_workers){
  
  if (length(TOGGLE_vaccination_strategy) == 0){return(vaccination_history = data.frame())}
  
  TOGGLE_vaccination_strategy = list(vaccine_delivery_start_date = 100,
                                     supply = c(0.2,0.5),
                                     strategy = list(
                                       list("older adults followed by all adults",c("60 to 110"),c("18 to 29","30 to 59")),
                                       list("all adults at the same time",c("18 to 29","30 to 59","60 to 110")),
                                       list("children before adults", c("0 to 4","5 to 17"), c("18 to 29","30 to 59","60 to 110"))                                                     )
                                     )
  check_configured_strategies <- TOGGLE_vaccination_strategy$strategy
  for(this_strategy in check_configured_strategies){
    if(grepl('[0-9]', substr(this_strategy[[1]],1,1))){stop("the label of your vaccination strategies should start with character descriptions NOT digits")}
    this_strategy <- this_strategy[-1]
    if(length(this_strategy[! this_strategy %in% this_strategy])>0){stop("you appear to have mistyped an age_group when defining your vaccination strategies")}
  }
  

  ### DELIVER TO ESSENTIAL WORKERS
  essential_worker_target <- population_by_risk_group %>%
    left_join(essential_workers, by = "age_group") %>%
    mutate(individuals = individuals * proportion) %>%
    mutate(proportion = individuals/sum(individuals)) %>% #calculate this age/comorb combination as a proportion of all essential workers
    mutate(doses_delivered = proportion * vaccine_delivery_capacity) #calculate doses_delivered on a day of full capacity delivery to essential workers
  if(round(sum(essential_worker_target$proportion),digits=2) != 1){stop("proportion calculatin on essential_worker_target not EQ 100%")}
  
  essential_worker_timeframe = floor(sum(essential_worker_target$individuals)/vaccine_delivery_capacity) #create sequence of days for full capacity delivery to essential workers
  time_sequence = seq(TOGGLE_vaccination_strategy$vaccine_delivery_start_date,TOGGLE_vaccination_strategy$vaccine_delivery_start_date+essential_worker_timeframe-1,by=1)
  if(length(time_sequence) != essential_worker_timeframe){stop("time sequence not aligning with timeframe for delivery of doses to essential workers")}
  
  essential_worker_delivery = crossing(time = time_sequence,essential_worker_target) 
  final_row_delivery =  sum(essential_worker_target$individuals) - sum(essential_worker_delivery$doses_delivered)
  final_row = essential_worker_target %>%
    mutate(doses_delivered = proportion * final_row_delivery,
           time = 100 + essential_worker_timeframe + 1)
  essential_worker_delivery <- rbind(essential_worker_delivery,final_row) %>%
    select(-proportion,-individuals) %>%
    mutate(phase = "essential_workers")
  if(round(sum(essential_worker_delivery$doses_delivered)) != round(sum(essential_worker_target$individuals))){stop("too many doses delivered to essential workers")}
  if(nrow(essential_worker_delivery[essential_worker_delivery$doses_delivered<0,])>0){stop("negative doses delivered to essential workers")}
  
  
  
  ### DELIVER OTHER STRATEGIES
  partial_dose_delivery_on_first_day = vaccine_delivery_capacity - final_row_delivery
  remainder_of_population_target <-population_by_risk_group %>%
    left_join(essential_workers, by = "age_group") %>%
    mutate(individuals = individuals * (1-proportion)) %>%
    select(-proportion)
    
  vaccination_strategy_combinations <- crossing(supply = TOGGLE_vaccination_strategy$supply,
                                                strategy = TOGGLE_vaccination_strategy$strategy) %>%
    mutate(phase = paste0("strategy_",row_number()))
  
  index = 0
  for (this_supply in TOGGLE_vaccination_strategy$supply){
    for (this_strategy_index in 1:length(TOGGLE_vaccination_strategy$strategy)){
      
      index = index + 1
      this_phase = paste0("strategy_",index)
      this_strategy = vaccination_strategy_combinations$strategy[this_strategy_index]
      this_population_target <- remainder_of_population_target
      
      if(this_supply > 1 | this_supply < 0){stop("vaccination supply not within 0 to 1")}
      
      this_population_target$priority = 1
      
      if(this_strategy == "uniform"){
        this_population_target$priority = 1
      } else if (this_strategy == "children"){
        this_population_target <- this_population_target %>%
          mutate(priority = case_when(
          ))
      }
      
    }
  
  }

  
  
}