require(ids)

generate_simulations <- function(
    this_configuration = list(
      setting = "Indonesia",
      R0 = 2,
      infection_derived_immunity = 1,
      
      vaccine_derived_immunity = 1,
      vaccine_delivery_start_date = 100,
      supply = 0.2,
      rollout_modifier = 1,
      phase = c("uniform", "step up", "step down"),
      daily_vaccine_delivery_realistic = FALSE,
      
      outcome_threshold = 2,
      gen_interval = 7,
      develop_outcome = 14,
      IR_outcome = 0.01
    ) #,
    ,
    ROUND_days_to_detection = 1
    # assign_run_ID = FALSE
){
  
  # length(this_configuration$setting)*length(this_configuration$vaccine_delivery_start_date)*
  # length(this_configuration$R0)*length(this_configuration$infection_derived_immunity)*
  # length(this_configuration$rollout_modifier)*length(this_configuration$vaccine_derived_immunity) *
  # length(this_configuration$daily_vaccine_delivery_realistic)
  
  LIST_strategy = list()
  if ("uniform" %in% this_configuration$phase){
    LIST_strategy[[length(LIST_strategy)+1]] <- 
      list("uniform",
                list(c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")))
  } 
  if ("older adults followed by all adults" %in% this_configuration$phase){
    LIST_strategy[[length(LIST_strategy)+1]] <- 
      list("older adults followed by all adults",
                list(c("60 to 110")),
                list(c("18 to 29","30 to 59")))
  }
  if ("adults then children" %in% this_configuration$phase){
    LIST_strategy[[length(LIST_strategy)+1]] <- 
      list("adults then children",
                list(c("18 to 29","30 to 59","60 to 110")),
                list(c("0 to 4","5 to 17")))
  }
  if ("children then adults" %in% this_configuration$phase){ 
    LIST_strategy[[length(LIST_strategy)+1]] <- 
      list("children then adults",
           list(c("0 to 4", "5 to 17")),
           list(c("18 to 29", "30 to 59", "60 to 110")))
  }
  if ("step up" %in% this_configuration$phase) {
    LIST_strategy[[length(LIST_strategy)+1]] <- 
      list("step up",
                list(c("0 to 4")),
                list(c("5 to 17")),
                list(c("18 to 29")),
                list(c("30 to 59")),
                list(c("60 to 110")))
  }
  if ("step down" %in% this_configuration$phase) { 
    LIST_strategy[[length(LIST_strategy)+1]] <- 
      list("step down",
           list(c("60 to 110")),
           list(c("30 to 59")),
           list(c("18 to 29")),
           list(c("5 to 17")),
           list(c("0 to 4")))
  }
  
  ship_log <- ship_log_key <- days_to_detection_key <- indicator_log <- data.frame()
  
  for (vaccine_derived_immunity in this_configuration$vaccine_derived_immunity){
    for(infection_derived_immunity in this_configuration$infection_derived_immunity){
      for (vaccine_delivery_start_date in this_configuration$vaccine_delivery_start_date){
        for (R0_to_fit in this_configuration$R0){
          
          days_to_detection_df = crossing(
            outcome_threshold = this_configuration$outcome_threshold,
            gen_interval = this_configuration$gen_interval,
            IR_outcome = this_configuration$IR_outcome,
            develop_outcome = this_configuration$develop_outcome,
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
            for (rollout_modifier in this_configuration$rollout_modifier){
              for (daily_vaccine_delivery_realistic in this_configuration$daily_vaccine_delivery_realistic){
                
                FLEET_ADMIRAL_OVERRIDE = list(
                  setting = this_configuration$setting,
                  days_to_detection = days_to_detection,
                  vaccine_delivery_start_date = vaccine_delivery_start_date,
                  
                  R0 = R0_to_fit,
                  infection_derived_immunity = infection_derived_immunity,
                  
                  supply = this_configuration$supply,
                  rollout_modifier = rollout_modifier,
                  daily_vaccine_delivery_realistic = daily_vaccine_delivery_realistic,
                  strategy = LIST_strategy,
                  vaccine_derived_immunity = vaccine_derived_immunity
                )
                
                source(paste0(gsub("/04_shiny","",getwd()),"/command_deck.R"), local = TRUE) #NB: 11/04/2024 12 seconds
                
                this_simulation_indicator <- indicator_delivery_within_time_horizon %>%
                  mutate(setting = FLEET_ADMIRAL_OVERRIDE$setting,
                         daily_vaccine_delivery_realistic = FLEET_ADMIRAL_OVERRIDE$daily_vaccine_delivery_realistic,
                         days_to_detection = days_to_detection)
                this_simulation <- incidence_log_tidy %>%
                  filter(time>0) %>%
                  ungroup() %>%
                  select(-simulation_time)
                
                
                # if (assign_run_ID == TRUE){
                  this_simulation_ID <- random_id(n = 1, bytes = 8)
                  this_simulation$run_ID = this_simulation_ID
                  
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
                  ship_log_key = rbind(ship_log_key, this_simulation_key)
                # }
                
                ship_log = rbind(ship_log,this_simulation)
                indicator_log = rbind(indicator_log,this_simulation_indicator)
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