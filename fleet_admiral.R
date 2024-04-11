
### The 'fleet admiral' runs the command_deck  multiple times to generate the data set underlying the Shiny 
require(ids)
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)} # load all functions


### RUN
################################################################################
result <- generate_simulations(
  LIST_setting = "Indonesia",
  
  #pathogen characteristics
  LIST_R0_to_fit = c(2,3,4,5), 
  LIST_infection_derived_immunity = c(0.75,1),
  
  #vaccination strategies
  LIST_vaccine_delivery_start_date = c(50,100,200),
  LIST_rollout_modifier = c(0.5,1,2),
  LIST_vaccine_derived_immunity = c(0.75,1),
  LIST_supply = c(0.2,0.5,0.8),
  LIST_daily_vaccine_delivery_realistic = c(TRUE,FALSE), #TBD if we need to run this for ALL permutations
  LIST_strategy = list(
    #list all strategies as individual lists (c(age groups), c(comorbidity status where TRUE = has a comorbidity))
    list("older adults followed by all adults",
         list(c("60 to 110")),
         list(c("18 to 29","30 to 59"))),
    list("adults then children",
         list(c("18 to 29","30 to 59","60 to 110")),
         list(c("0 to 4","5 to 17"))),
    list("children then adults", 
         list(c("0 to 4","5 to 17")), 
         list(c("18 to 29","30 to 59","60 to 110"))),
    list("step up",
         list(c("0 to 4")),
         list(c("5 to 17")),
         list(c("18 to 29")),
         list(c("30 to 59")),
         list(c("60 to 110"))),
    list("step down",
         list(c("60 to 110")),
         list(c("30 to 59")),
         list(c("18 to 29")),
         list(c("5 to 17")),
         list(c("0 to 4"))),
    list("uniform",
         list(c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")))
  ),
  
  #parameters impacting days to detection
  LIST_outcome_threshold = c(1,2,5,10),
  LIST_gen_interval = c(7, 14, 28),
  LIST_IR_outcome = c(0.01, # COVID-19 WT and influenza like
                      0.1,  # diptheria and SARS like
                      0.25, # Lassa fever and MERS like
                      0.5,  # TB, cholera, JEV and HIV like
                      0.65  # plague and Ebola like
                      #NB: need to include est for presentations!
  ),
  LIST_develop_outcome = c(7, 14, 28),
  ROUND_days_to_detection = 1
)
#_______________________________________________________________________________


### SAVE
################################################################################
ship_log = result$ship_log
ship_log_key = result$ship_log_key
indicator_log = result$indicator_log
days_to_detection_key = result$days_to_detection_key

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
