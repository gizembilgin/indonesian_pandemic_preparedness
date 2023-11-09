
configure_vaccination_history <- function(TOGGLE_vaccination_strategy = list(),
                                          vaccine_delivery_capacity = load_setting_characteristics$vaccine_delivery_capacity,
                                          essential_workers = loaded_setting_characteristics$essential_workers){
  
  if (length(TOGGLE_vaccination_strategy) == 0){return(vaccination_history = data.frame())}
  
  vaccination_strategy_combinations <- crossing(supply = TOGGLE_vaccination_strategy$supply,
                                                strategy = TOGGLE_vaccination_strategy$strategy) %>%
    mutate(phase = paste0("strategy_",row_number()))

  
  
  ### DELIVER TO ESSENTIAL WORKERS
  
  
  ### DELIVER OTHER STRATEGIES
  for (index in nrow(vaccination_strategy_combinations)){
    this_supply   = vaccination_strategy_combinations$supply[index]
    this_strategy = vaccination_strategy_combinations$strategy[index]
    this_phase    = vaccination_strategy_combinations$phase[index]
    
    if(this_supply > 1 | this_supply < 0){stop("vaccination supply not within 0 to 1")}
    if (! this_strategy %in% c("uniform","children","elderly")){stop("we have not configured this vaccination strategy")}
    
    
    
  }

  
  
}