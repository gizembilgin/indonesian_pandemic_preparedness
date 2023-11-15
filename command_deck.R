### The 'command deck' runs all sub-scripts of the model


### (1/2) Setup                 
################################################################################
# load libraries
require(tidyverse);require(deSolve)
options(scipen = 1000)

# load all functions
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}

# user toggles
age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")

TOGGLE_setting = "Indonesia"

# pathogen characteristics
TOGGLE_R0_to_fit = 2
TOGGLE_average_symptomatic_period = 7
TOGGLE_average_exposed_period = 7
TOGGLE_prevalence_symptoms = rep(0.8,rep(length(age_group_labels)))
TOGGLE_reduced_infectiousness_asymptomatic = 0.5
TOGGLE_susceptibility = rep(0.8,rep(length(age_group_labels)))
TOGGLE_average_immune_period = 365*10
TOGGLE_vaccination_strategy = list(vaccine_delivery_start_date = 100,
                                   supply = c(0.2,0.5),
                                   strategy = list(
                                     list("older adults followed by all adults",
                                          list(c("60 to 110"),c(0,1)),
                                          list(c("18 to 29","30 to 59"),c(0,1))),
                                     list("all adults at the same time",
                                          list(c("18 to 29","30 to 59","60 to 110"),c(0,1))),
                                     list("children before adults", 
                                          list(c("0 to 4","5 to 17"),c(0,1)), 
                                          list(c("18 to 29","30 to 59","60 to 110"),c(0,1)))                                                     )
)

TOGGLE_increased_risk = 2 #NOT CURRENTLY USED
TOGGLE_NPI = 0.5
TOGGLE_infection_derived_immunity = 1
TOGGLE_vaccine_derived_immunity = 0.8

TOGGLE_introduction_day = 0 #NB: just change length of run if later
TOGGLE_introductions = 0.00001 #percentage of pop of introductions on day

#_______________________________________________________________________________


### (2/2) Run model            
################################################################################
loaded_setting_characteristics <- load_setting(this_setting = TOGGLE_setting)

inital_state <- configure_inital_state(
  introduction = TOGGLE_introductions,
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
  age_group_labels=age_group_labels,
  VE=TOGGLE_vaccine_derived_immunity,
  num_age_groups=num_age_groups)

incidence_log_tidy<- run_disease_model()


to_plot <- incidence_log_tidy %>%
  group_by(time) %>%
  summarise(incidence = sum(incidence))
ggplot(to_plot) + 
  geom_point(aes(x=time,y=incidence))
#_______________________________________________________________________________