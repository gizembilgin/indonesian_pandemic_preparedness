#Functional plotting for multiple runs of the command_deck (e.g., fleet_admiral)

### SETUP
require(tidyverse) require(ggpubr)
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
    this_output = "cases",    #options: cases, deaths
    TOGGLES_project_severe_disease = list(),
    free_yaxis = FALSE,
    var_1_range = c(2,3,4),
    var_2_range = NA,
    default_configuration =
      list(
        R0 = 2,
        vaccine_delivery_start_date = 100,
        phase = c(
          "older adults followed by all adults",
          "children before adults",
          "all adults at the same time",
          "essential workers",
          "no vaccine"
        ),
        supply = c(0.2),
        infection_derived_immunity = 1,
        rollout_modifier = 2,
        vaccine_derived_immunity = 1
      ),
    load_simulations = TRUE, #load simulations for each run
    #options: 0 (no), 1 (yes)
    display_impact_heatmap = 1, 
    display_var_1 = 1,
    colour_essential_workers_phase = 1, 
    display_vaccine_availability = 1, 
    display_end_of_essential_worker_delivery = 1 
)
################################################################################



### Plot to find where vaccine prioritisation strategies make a difference
plot_simulations (
  var_1 = "R0",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = "vaccine_delivery_start_date",
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_output = "cases",    #options: cases, deaths
  TOGGLES_project_severe_disease = list(),
  free_yaxis = FALSE,
  var_1_range = c(2,3,4),
  var_2_range = c(50,100,200),
  default_configuration =
    list(
      R0 = 2,
      vaccine_delivery_start_date = 100,
      phase = c(
        "all adults at the same time",
        "essential workers",
        "no vaccine"
      ),
      supply = c(0.8),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1
    ),
  load_simulations = TRUE, #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_essential_workers_phase = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1, 
  display_end_of_essential_worker_delivery = 0 
)


#SM fig 1
plot_simulations (
  var_1 = "vaccine_delivery_start_date",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = NA,
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_output = "cases",    #options: cases, deaths
  TOGGLES_project_severe_disease = list(),
  free_yaxis = FALSE,
  var_1_range = c(50,100),
  var_2_range = NA,
  default_configuration =
    list(
      R0 = 2,
      vaccine_delivery_start_date = 100,
      phase =  c("older adults followed by all adults",
                 "children before adults",
                 "all adults at the same time",
                 "essential workers",
                 "no vaccine" ),
      supply = c(0.8),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1
    ),
  load_simulations = TRUE, #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_essential_workers_phase = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1, 
  display_end_of_essential_worker_delivery = 1,
  display_impact_heatmap = 0
)


#SM fig 2
plot_simulations (
  var_1 = "R0",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = "rollout_modifier",
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_output = "cases",    #options: cases, deaths
  TOGGLES_project_severe_disease = list(),
  free_yaxis = FALSE,
  var_1_range = c(2,3,4),
  var_2_range = NA,
  default_configuration =
    list(
      R0 = 2,
      vaccine_delivery_start_date = 100,
      phase =  c(
                 "all adults at the same time",
                 "essential workers",
                 "no vaccine" ),
      supply = c(0.8),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1
    ),
  load_simulations = TRUE, #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_essential_workers_phase = 0,
  display_var_1 = 0,
  display_vaccine_availability = 1, 
  display_end_of_essential_worker_delivery = 1,
  display_impact_heatmap = 0
)
################################################################################



### Plot to find how prioritisation strategies are influenced by severity
plot_simulations (
  var_1 = "pathogen",             #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
  var_2 = NA,
  yaxis_title ="incidence", #options: incidence, cumulative_incidence, cumulative_incidence_averted
  this_output = "deaths",    #options: cases, deaths
  TOGGLES_project_severe_disease = list(
    point_estimate =  1 / 100,
    age_distribution = c("SARS", "Plague", "Influenza 1918"),
    VE_severe_disease = 1,
    comorb_increased_risk = 1
  ),
  free_yaxis = FALSE,
  var_1_range = NA,
  var_2_range = NA,
  default_configuration =
    list(
      R0 = 2,
      vaccine_delivery_start_date = 100,
      phase =  c(
        "older adults followed by all adults",
        "children before adults",
        "all adults at the same time",
        "essential workers",
        "no vaccine" ),
      supply = c(0.8),
      infection_derived_immunity = 1,
      rollout_modifier = 2,
      vaccine_derived_immunity = 1
    ),
  load_simulations = TRUE, #load simulations for each run
  #options: 0 (no), 1 (yes)
  colour_essential_workers_phase = 1,
  display_var_1 = 0,
  display_vaccine_availability = 1
)
