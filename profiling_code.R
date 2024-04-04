
### TIME one run
system.time({source("command_deck.R")})
# user  system elapsed 
# 30.46    0.70   31.34 

### PROFILE it
profvis::profvis({source("command_deck.R")})

profvis::profvis({
  vaccination_history_permutations <- configure_vaccination_history(LIST_vaccination_strategies = TOGGLE_vaccination_strategy)
})
profvis::profvis({
  incidence_log_tidy<- run_disease_model(time_horizon = TOGGLE_simulation_days,
                                         vaccination_strategies = TOGGLE_vaccination_strategy)
})
