### The 'command deck' runs all sub-scripts of the model


### (1/2) Setup                 
################################################################################
# load libraries
require(tidyverse)

# load all functions
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}

# user toggles
age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")

TOGGLE_setting = "Indonesia"
TOGGLE_R0_to_fit = 2
TOGGLE_average_symptomatic_period = 7
TOGGLE_prevalence_symptoms = rep(0.8,rep(length(age_group_labels)))
TOGGLE_reduced_infectiousness_asymptomatic = 0.5
TOGGLE_susceptibility = rep(0.8,rep(length(age_group_labels)))



#_______________________________________________________________________________


### (2/2) Run model            
################################################################################
loaded_setting_characteristics <- load_setting(this_setting = TOGGLE_setting)
inital_state <- configure_inital_state()
fit_beta_to_R0(
  R0_to_fit = TOGGLE_R0_to_fit,
  this_average_symptomatic_period = TOGGLE_average_symptomatic_period,
  this_prevalence_symptoms = TOGGLE_prevalence_symptoms,
  this_reduced_infectiousness_asymptomatic = TOGGLE_reduced_infectiousness_asymptomatic,
  this_susceptibility = TOGGLE_susceptibility
)
#_______________________________________________________________________________