#Functional plotting for multiple runs of the command_deck (e.g., fleet_admiral)

### SETUP
require(tidyverse); require(ggpubr);require(shiny)
options(scipen = 1000)
#rm(list = ls())
TOGGLE_setting = "Indonesia" #options: "Indonesia" or name a province of Indonesia
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}
loaded_setting_characteristics <- load_setting(this_setting = TOGGLE_setting)


### Plot influence of one variable at a time 
plot_simulations (
    var_1 = "R0",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
    var_2 = NA,
    yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
    this_outcome = "infections",    #options: infections, deaths
    TOGGLES_project_deaths = list(),
    free_yaxis = FALSE,
    var_1_range = c(2,3,4),
    var_2_range = NA,
    default_configuration =
      list(
        R0 = 2,            # basic reproduction number
        outcome_threshold = 2, # threshold number of this outcome for detection
        gen_interval = 7,      # generation interval (days)
        IR_outcome = 0.01,     # incidence rate for this outcome
        detection_outcome = "deaths",
        develop_outcome = 14,  # time to developing outcome (days)
        vaccine_delivery_start_date = 100,
        vaccine_acceptance_overwrite = data.frame(),
        daily_vaccine_delivery_realistic = TRUE,
        phase = c(
          #"older adults followed by all adults",
          #"adults then children",               
          #"children then adults",                
          "youngest to oldest",                            
          "oldest to youngest",                           
          "uniform", 
          "healthcare workers",
          "no vaccine"
        ),
        supply = c(0.2),
        infection_derived_immunity = 1,
        rollout_modifier = 2,
        vaccine_derived_immunity = 1
      ),
    simulations_source = "generate", #load simulations for each run
    #options: 0 (no), 1 (yes)
    display_impact_heatmap = 1, 
    display_var_1 = 1,
    colour_healthcare_workers_phase = 1, 
    display_vaccine_availability = 1, 
    display_end_of_healthcare_worker_delivery = 1 
)
################################################################################



### Plot to find where vaccine prioritisation strategies make a difference
plot_simulations (
  var_1 = "R0",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = "vaccine_delivery_start_date",
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_outcome = "infections",    #options: infections, deaths,presentations
  TOGGLES_project_deaths = list(),
  free_yaxis = FALSE,
  var_1_range = c(2,3,4),
  var_2_range = c(50,100,200),
  default_configuration =
    list(
      R0 = 2,
      outcome_threshold = 2, # threshold number of this outcome for detection
      gen_interval = 7,      # generation interval (days)
      IR_outcome = 0.01,     # incidence rate for this outcome
      detection_outcome = "deaths",
      develop_outcome = 14,  # time to developing outcome (days)
      vaccine_delivery_start_date = 100,
      # vaccine_acceptance_overwrite = data.frame(age_group =  c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110"),
      #                                           overwrite = rep(0.01,5)),
      vaccine_acceptance_overwrite = data.frame(),
      daily_vaccine_delivery_realistic = TRUE,
      phase = c(
        "uniform",
        "healthcare workers",
        "no vaccine"
      ),
      supply = c(0.2),
      infection_derived_immunity = 1,
      rollout_modifier = 1,
      vaccine_derived_immunity = 1
    ),
  simulations_source = "generate", #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_healthcare_workers_phase = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1, 
  display_end_of_healthcare_worker_delivery = 0 
)


#SM fig 1
plot_simulations (
  var_1 = "vaccine_delivery_start_date",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = NA,
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_outcome = "infections",    #options: infections, deaths
  TOGGLES_project_deaths = list(),
  free_yaxis = FALSE,
  var_1_range = c(50,100),
  var_2_range = NA,
  default_configuration =
    list(
      R0 = 2,
      outcome_threshold = 2, # threshold number of this outcome for detection
      gen_interval = 7,      # generation interval (days)
      IR_outcome = 0.01,     # incidence rate for this outcome
      detection_outcome = "deaths",
      develop_outcome = 14,  # time to developing outcome (days)
      vaccine_delivery_start_date = 100,
      phase =  c( #"older adults followed by all adults",
                  #"adults then children",               
                  #"children then adults",                
                  "youngest to oldest",                            
                  "oldest to youngest",                           
                  "uniform", 
                  "healthcare workers",
                  "no vaccine" ),
      supply = c(0.2),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1
    ),
  simulations_source = "generate", #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_healthcare_workers_phase = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1, 
  display_end_of_healthcare_worker_delivery = 1,
  display_impact_heatmap = 0
)


#SM fig 2
plot_simulations (
  var_1 = "R0",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = "rollout_modifier",
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_outcome = "infections",    #options: infections, deaths
  TOGGLES_project_deaths = list(),
  free_yaxis = FALSE,
  var_1_range = c(2,3,4),
  var_2_range = NA,
  default_configuration =
    list(
      R0 = 2,
      detection_outcome = "deaths",
      outcome_threshold = 2, # threshold number of this outcome for detection
      gen_interval = 7,      # generation interval (days)
      IR_outcome = 0.01,     # incidence rate for this outcome
      develop_outcome = 14,  # time to developing outcome (days)
      vaccine_delivery_start_date = 100,
      phase =  c(
                 "uniform",
                 "healthcare workers",
                 "no vaccine" ),
      supply = c(0.8),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1
    ),
  simulations_source = "load", #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_healthcare_workers_phase = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1, 
  display_end_of_healthcare_worker_delivery = 1,
  display_impact_heatmap = 0
)
################################################################################



### Plot to find how prioritisation strategies are influenced by severity
plot_simulations (
  var_1 = "pathogen",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = NA,
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_outcome = "deaths",    #options: infections, deaths
  TOGGLES_project_deaths = list(
    point_estimate =  1 / 100,
    age_distribution = c("Plague", "Diptheria","COVID-19 WT","Influenza 1918"),
    VE_death = 1,
    comorb_increased_risk = 1
  ),
  free_yaxis = FALSE,
  var_1_range = NA,
  var_2_range = NA,
  default_configuration =
    list(
      R0 = 2,
      detection_outcome = "deaths",
      outcome_threshold = 2, # threshold number of this outcome for detection
      gen_interval = 7,      # generation interval (days)
      IR_outcome = 0.01,     # incidence rate for this outcome
      develop_outcome = 14,  # time to developing outcome (days)
      vaccine_delivery_start_date = 100,
      vaccine_acceptance_overwrite = data.frame(),
      phase =  c(
        "youngest to oldest",                            
        "oldest to youngest", 
        "uniform" ,
        "healthcare workers",
        "no vaccine" ),
      supply = c(0.2),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1,
      daily_vaccine_delivery_realistic = FALSE
    ),
  simulations_source = "generate", #load simulations for each run
  #options: 0 (no), 1 (yes)
  display_severity_curve = 1, 
  colour_healthcare_workers_phase = 0,
  display_end_of_healthcare_worker_delivery = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1
)
