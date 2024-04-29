
### The 'fleet admiral' runs the command_deck  multiple times to generate the data set underlying the Shiny 
require(ids)
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)} # load all functions

### RUN
################################################################################
result <- generate_simulations(
  this_configuration = list(
    setting = "Indonesia",
    
    #pathogen characteristics
    R0 = c(2,3,4,5), 
    infection_derived_immunity = c(0.75,1),
    
    #vaccination strategies
    vaccine_delivery_start_date = c(50,100,200),
    rollout_modifier = c(0.5,1,2),
    vaccine_derived_immunity = c(0.75,1),
    supply = c(0.2,0.5,0.8),
    daily_vaccine_delivery_realistic = c(TRUE,FALSE), #TBD if we need to run this for ALL permutations
    strategy_name = c("older adults followed by all adults","adults then children","children then adults", 
                      "youngest to oldest", "oldest to youngest","uniform"),

    #parameters impacting days to detection
    outcome_threshold = c(1,2,5,10),
    gen_interval = c(7, 14, 28),
    IR_outcome = c(0.01, # COVID-19 WT and influenza like
                        0.1,  # diptheria and SARS like
                        0.25, # Lassa fever and MERS like
                        0.5,  # TB, cholera, JEV and HIV like
                        0.65  # plague and Ebola like
                        #NB: need to include est for presentations!
                   ),
    develop_outcome = c(7, 14, 28), 
    ROUND_days_to_detection = 1
  ),
  assign_run_ID = TRUE
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
