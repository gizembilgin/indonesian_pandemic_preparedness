### The 'command deck' runs all sub-scripts of the model


### Setup                 
################################################################################
# load libraries
require(tidyverse);require(deSolve); require(ggpubr)
options(scipen = 1000)
#rm(list = ls())

# load all functions
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}

# user toggles
age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
TOGGLE_setting = "Indonesia" #options: "Indonesia" or name a province of Indonesia

#simulation configuration
TOGGLE_simulation_days = 365+365 #scope of analysis to one year + assume detected within a year of introduction
TOGGLE_detection_prevalence = 0.0001 #percentage prevalence on day of detection
TOGGLE_NPI = 0 #0 means no NPI since used as (1-NPI)*transmission

# pathogen characteristics
TOGGLE_R0_to_fit = 2
TOGGLE_average_symptomatic_period = 7
TOGGLE_average_exposed_period = 7

TOGGLES_project_severe_disease = list(
  point_estimate = 0.05/100,
  # age_distribution = data.frame(
  #   age_group = age_group_labels,
  #   relative_risk = c(0.1,0.5,1.4,2.7,10.4)
  # )
  age_distribution = "Plague",
  VE_severe_disease = 1,
  comorb_increased_risk = 2
  
)

TOGGLE_reduced_infectiousness_asymptomatic = 1
TOGGLE_prevalence_symptoms = rep(1,rep(length(age_group_labels)))
TOGGLE_susceptibility = rep(1,rep(length(age_group_labels)))

TOGGLE_average_immune_period = 365*10
TOGGLE_infection_derived_immunity = 1

#vaccination strategy
TOGGLE_vaccine_derived_immunity = 0.8
TOGGLE_vaccination_strategy = list(vaccine_delivery_start_date = 100, #NB: COVID-19 was closer to 365
                                   supply = c(0.1,0.2,0.3,0.4), #list all supply scenarios
                                   rollout_modifier = 1,
                                   daily_vaccine_delivery_realistic = FALSE,
                                   strategy = list( #list all strategies as individual lists (c(age groups), c(comorbidity status where 1 = has a comorbidity))
                                     list("older adults followed by all adults",
                                          list(c("60 to 110"),c(0,1)),
                                          list(c("18 to 29","30 to 59"),c(0,1))),
                                     list("all adults at the same time",
                                          list(c("18 to 29","30 to 59","60 to 110"),c(0,1))),
                                     list("children before adults", 
                                          list(c("0 to 4","5 to 17"),c(0,1)), 
                                          list(c("18 to 29","30 to 59","60 to 110"),c(0,1)))                                                     )
)

if (exists("FLEET_ADMIRAL_OVERRIDE")){
  if ("setting" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_setting = FLEET_ADMIRAL_OVERRIDE$setting
  if ("vaccine_delivery_start_date" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_vaccination_strategy$vaccine_delivery_start_date = FLEET_ADMIRAL_OVERRIDE$vaccine_delivery_start_date
  
  if ("R0" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_R0_to_fit = FLEET_ADMIRAL_OVERRIDE$R0
  if ("infection_derived_immunity" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_infection_derived_immunity = FLEET_ADMIRAL_OVERRIDE$infection_derived_immunity
  
  if ("supply" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_vaccination_strategy$supply = FLEET_ADMIRAL_OVERRIDE$supply
  if ("rollout_modifier" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_vaccination_strategy$rollout_modifier = FLEET_ADMIRAL_OVERRIDE$rollout_modifier
  if ("daily_vaccine_delivery_realistic" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_vaccination_strategy$daily_vaccine_delivery_realistic = FLEET_ADMIRAL_OVERRIDE$daily_vaccine_delivery_realistic
  if ("strategy" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_vaccination_strategy$strategy = FLEET_ADMIRAL_OVERRIDE$strategy
  if ("vaccine_derived_immunity" %in% names(FLEET_ADMIRAL_OVERRIDE)) TOGGLE_vaccine_derived_immunity = FLEET_ADMIRAL_OVERRIDE$vaccine_derived_immunity
}
#_______________________________________________________________________________



### Run model            
################################################################################
loaded_setting_characteristics <- load_setting(this_setting = TOGGLE_setting)

inital_state <- configure_inital_state(
  detection_prevalence = TOGGLE_detection_prevalence,
  average_symptomatic_period = TOGGLE_average_symptomatic_period,
  average_exposed_period  = TOGGLE_average_exposed_period 
)

fitted_beta <- fit_beta_to_R0(
  R0_to_fit = TOGGLE_R0_to_fit,
  this_average_symptomatic_period = TOGGLE_average_symptomatic_period,
  this_prevalence_symptoms = TOGGLE_prevalence_symptoms,
  this_reduced_infectiousness_asymptomatic = TOGGLE_reduced_infectiousness_asymptomatic,
  this_susceptibility = TOGGLE_susceptibility
)

parameters = list(
  suscept = TOGGLE_susceptibility,
  beta=fitted_beta,
  NPI=TOGGLE_NPI,
  contact_matrix=loaded_setting_characteristics$contact_matrix,
  lota=TOGGLE_reduced_infectiousness_asymptomatic,
  gamma=TOGGLE_prevalence_symptoms,
  lambda=1/TOGGLE_average_exposed_period,
  delta=1/TOGGLE_average_symptomatic_period,
  omega=1/TOGGLE_average_immune_period,
  rho=TOGGLE_infection_derived_immunity,
  VE=TOGGLE_vaccine_derived_immunity,
  J=length(age_group_labels)
)

workshop <- run_disease_model(
  time_horizon = TOGGLE_simulation_days,
  vaccination_strategies = TOGGLE_vaccination_strategy
)
incidence_log_tidy <- workshop$incidence_log_tidy
indicator_delivery_within_time_horizon <- workshop$indicator_delivery_within_time_horizon
rm(workshop)

# severe_disease_log_tidy <- project_severe_disease(
#   point_estimate = TOGGLE_severe_disease_point_estimate,
#   age_distribution = TOGGLE_severe_disease_age_distribution,
#   VE = TOGGLE_severe_disease_VE,
#   comorb_increased_risk = TOGGLE_severe_disease_comorb_increased_risk,
#   this_incidence_log_tidy = incidence_log_tidy,
#   this_pop = loaded_setting_characteristics$population_by_comorbidity
# )
#_______________________________________________________________________________
