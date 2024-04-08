
### The 'fleet admiral' runs the command_deck  multiple times to generate the data set underlying the Shiny 
require(ids)
# load all functions
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}


### CONFIGURE
################################################################################
#scenario configuration
LIST_setting = c("Indonesia")
LIST_vaccine_delivery_start_date = c(50,100,200)

#pathogen characteristics
LIST_R0_to_fit = c(2,3,4)  #  c(1,2,4,6,8) 
LIST_infection_derived_immunity = c(0.75,1)

#vaccination strategies
LIST_rollout_modifier = c(0.5,1,2)
LIST_vaccine_derived_immunity = c(0.75,1)
LIST_supply = c(0.2,0.5,0.8)
LIST_daily_vaccine_delivery_realistic = c(TRUE,FALSE) #TBD if we need to run this for ALL permutations
LIST_strategy = list(
  #list all strategies as individual lists (c(age groups), c(comorbidity status where 1 = has a comorbidity))
  list("older adults followed by all adults",
       list(c("60 to 110"),c(0,1)),
       list(c("18 to 29","30 to 59"),c(0,1))),
  list("adults then children",
       list(c("18 to 29","30 to 59","60 to 110"),c(0,1)),
       list(c("0 to 4","5 to 17"),c(0,1))),
  list("children then adults", 
       list(c("0 to 4","5 to 17"),c(0,1)), 
       list(c("18 to 29","30 to 59","60 to 110"),c(0,1))),
  list("step up",
       list(c("0 to 4"),c(0,1)),
       list(c("5 to 17"),c(0,1)),
       list(c("18 to 29"),c(0,1)),
       list(c("30 to 59"),c(0,1)),
       list(c("60 to 110"),c(0,1))),
  list("step down",
       list(c("60 to 110"),c(0,1)),
       list(c("30 to 59"),c(0,1)),
       list(c("18 to 29"),c(0,1)),
       list(c("5 to 17"),c(0,1)),
       list(c("0 to 4"),c(0,1))),
  list("uniform",
       list(c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110"),c(0,1)))
  )

#parameters impacting days to detection
LIST_outcome_threshold = c(1,2,5,10)
LIST_gen_interval = c(7, 14, 28)
LIST_IR_outcome = c(0.01, # COVID-19 WT and influenza like
                    0.1,  # diptheria and SARS like
                    0.25, # Lassa fever and MERS like
                    0.5,  # TB, cholera, JEV and HIV like
                    0.65  # plague and Ebola like
                    #NB: need to include est for presentations!
                    )
LIST_develop_outcome = c(7, 14, 28)
ROUND_days_to_detection = 1

# LIST_outcome_threshold = c(2)
# LIST_gen_interval = c(7)
# LIST_IR_outcome = c(0.01, 0.1, 0.25, 0.5, 0.65)
# LIST_develop_outcome = c(14)
# ROUND_days_to_detection = 1
#_______________________________________________________________________________


### RUN
################################################################################
length(LIST_setting)*length(LIST_vaccine_delivery_start_date)*
  length(LIST_R0_to_fit)*length(LIST_infection_derived_immunity)*
  length(LIST_rollout_modifier)*length(LIST_vaccine_derived_immunity) *
  length(LIST_daily_vaccine_delivery_realistic)

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
                 
                 source("command_deck.R") #NB: 05/04/2024 30 seconds
                 
                 this_simulation_ID <- random_id(n = 1, bytes = 8)
                 
                 this_simulation <- incidence_log_tidy %>%
                   mutate(run_ID = this_simulation_ID) %>%
                   filter(time>0) %>%
                   select(-simulation_time)
                 
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
rm(this_simulation, this_simulation_indicator)

time_of_result = Sys.time()
time_of_result = gsub(':','-',time_of_result)
save(days_to_detection_key,file = paste0("04_shiny/x_results/days_to_detection_key",time_of_result,".Rdata"))
save(ship_log_key,file = paste0("04_shiny/x_results/ship_log_key",time_of_result,".Rdata"))
save(ship_log,file = paste0("04_shiny/x_results/ship_log",time_of_result,".Rdata"))
indicator_log <- unique(indicator_log)
save(indicator_log,file = paste0("04_shiny/x_results/indicator_log",time_of_result,".Rdata"))
save.image(file = paste0("04_shiny/x_results/workspace_image_",time_of_result,".Rdata"))

rm(FLEET_ADMIRAL_OVERRIDE)
#_______________________________________________________________________________


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