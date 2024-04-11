generate_simulations <- function(
    
    LIST_setting = "Indonesia",
    LIST_R0_to_fit = 2,
    LIST_infection_derived_immunity = 1,  
    
    LIST_vaccine_derived_immunity = 1,
    LIST_vaccine_delivery_start_date = 100,
    LIST_rollout_modifier = 1,
    LIST_daily_vaccine_delivery_realistic = FALSE,

    LIST_outcome_threshold = 2,
    LIST_gen_interval = 7,
    LIST_IR_outcome = 0.01,
    LIST_develop_outcome = 14,
    ROUND_days_to_detection = 1
    
){
  
  # length(LIST_setting)*length(LIST_vaccine_delivery_start_date)*
  # length(LIST_R0_to_fit)*length(LIST_infection_derived_immunity)*
  # length(LIST_rollout_modifier)*length(LIST_vaccine_derived_immunity) *
  # length(LIST_daily_vaccine_delivery_realistic)
  
  ship_log <- ship_log_key <- days_to_detection_key <- indicator_log <- data.frame()
  
  for (setting in LIST_setting){
    for (vaccine_derived_immunity in LIST_vaccine_derived_immunity){
      for(infection_derived_immunity in LIST_infection_derived_immunity){
        for (vaccine_delivery_start_date in LIST_vaccine_delivery_start_date){
          for (R0_to_fit in LIST_R0_to_fit){
            
            days_to_detection_df = crossing(
              outcome_threshold = LIST_outcome_threshold,
              gen_interval = LIST_gen_interval,
              IR_outcome = LIST_IR_outcome,
              develop_outcome = LIST_develop_outcome,
              R0 = R0_to_fit
            ) %>%
              mutate(days_to_detection = estimate_days_to_detection(outcome_threshold,gen_interval,IR_outcome,develop_outcome,R0),
                     #days_to_detection = round(days_to_detection)) %>%
                     days_to_detection = round(days_to_detection/ROUND_days_to_detection)*ROUND_days_to_detection) %>%
              arrange(days_to_detection)
            # ggplot(workshop) + geom_histogram(aes(x=days_to_detection))
            # length(unique(workshop$days_to_detection)) #length ~ 85 with ROUND_days_to_detection == 1; and ~ 25 with ROUND_days_to_detection == 7
            
            days_to_detection_key <- rbind(days_to_detection_key,days_to_detection_df)
            
            for(days_to_detection in unique(days_to_detection_df$days_to_detection)){
              for (rollout_modifier in LIST_rollout_modifier){
                for (daily_vaccine_delivery_realistic in LIST_daily_vaccine_delivery_realistic){
                  
                  
                  FLEET_ADMIRAL_OVERRIDE = list(
                    setting = setting,
                    days_to_detection = days_to_detection,
                    vaccine_delivery_start_date = vaccine_delivery_start_date,
                    
                    R0 = R0_to_fit,
                    infection_derived_immunity = infection_derived_immunity,
                    
                    supply = LIST_supply,
                    rollout_modifier = rollout_modifier,
                    daily_vaccine_delivery_realistic = daily_vaccine_delivery_realistic,
                    strategy = LIST_strategy,
                    vaccine_derived_immunity = vaccine_derived_immunity
                  )
                  
                  source("command_deck.R") #NB: 11/04/2024 12 seconds
                  
                  this_simulation_ID <- random_id(n = 1, bytes = 8)
                  
                  this_simulation <- incidence_log_tidy %>%
                    mutate(run_ID = this_simulation_ID) %>%
                    filter(time>0) %>%
                    ungroup() %>%
                    select(-simulation_time,-comorbidity) #as run without comorbidity
                  
                  #save as a separate key rather than columns on the dataframe to save space
                  this_simulation_key <- data.frame(
                    run_ID = this_simulation_ID,
                    setting = FLEET_ADMIRAL_OVERRIDE$setting,
                    vaccine_delivery_start_date = FLEET_ADMIRAL_OVERRIDE$vaccine_delivery_start_date,
                    R0 = FLEET_ADMIRAL_OVERRIDE$R0,
                    infection_derived_immunity = FLEET_ADMIRAL_OVERRIDE$infection_derived_immunity,
                    rollout_modifier = FLEET_ADMIRAL_OVERRIDE$rollout_modifier,
                    daily_vaccine_delivery_realistic = FLEET_ADMIRAL_OVERRIDE$daily_vaccine_delivery_realistic,
                    vaccine_derived_immunity = FLEET_ADMIRAL_OVERRIDE$vaccine_derived_immunity,
                    days_to_detection = days_to_detection
                  )
                  
                  this_simulation_indicator <- indicator_delivery_within_time_horizon %>%
                    mutate(setting = FLEET_ADMIRAL_OVERRIDE$setting,
                           daily_vaccine_delivery_realistic = FLEET_ADMIRAL_OVERRIDE$daily_vaccine_delivery_realistic,
                           days_to_detection = days_to_detection)
                  
                  ship_log = rbind(ship_log,this_simulation)
                  ship_log_key = rbind(ship_log_key, this_simulation_key)
                  indicator_log = rbind(indicator_log,this_simulation_indicator)
                  
                }
              }
            }
          }
        }
      }
    }
  }
  
  result <- list(
    ship_log = ship_log,
    ship_log_key = ship_log_key,
    indicator_log = indicator_log,
    days_to_detection_key = days_to_detection_key
  )
  
  return(result)
}




###
#require(parallel); require(foreach)
# CLUSTER <- parallel::makeCluster(TOGGLE_clusterNumber) # create cluster
# doParallel::registerDoParallel(CLUSTER) # activate cluster
# 
# CommandDeck_result_long <- foreach::foreach(
#   model_run_number = c(1:TOGGLE_clusterNumber),
#   .packages = c("tidyverse","ids"),
#   .combine = rbind,
#   .inorder = FALSE
# )  %dopar% {
#   
#   CEA_worker(
#     numberOfRunsPerCluster,
#     CEA_risk_group,
#     LIST_CEA_settings,
#     LIST_perspectives,
#     LIST_booster_vax_scenarios,
#     LIST_antiviral_elig_groups,
#     LIST_antiviral_types,
#     DECISION_sampling_strategy,
#     DECISION_include_net,
#     TOGGLE_uncertainty,
#     TOGGLE_longCOVID,
#     LIST_discounting_rate,
#     LIST_antiviral_cost_scenario,
#     TORNADO_PLOT_OVERRIDE
#   )
#   
# }
# })
# 
# parallel::stopCluster(CLUSTER)