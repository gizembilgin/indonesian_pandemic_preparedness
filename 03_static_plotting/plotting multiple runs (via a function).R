#Functional plotting for multiple runs of the command_deck (e.g., fleet_admiral)

### Plot influence of one variable at a time 
plot_simulations(ship_log_completed,"R0", yaxis_title ="incidence",display_vaccine_availability = 1, display_end_of_essential_worker_delivery = 1)
plot_simulations(ship_log_completed,"vaccine_delivery_start_date", yaxis_title ="incidence")
#plot_simulations(ship_log_completed,"supply", yaxis_title ="incidence")
plot_simulations(ship_log_completed,"infection_derived_immunity", yaxis_title ="incidence")
plot_simulations(ship_log_completed,"rollout_modifier", yaxis_title ="incidence")
plot_simulations(ship_log_completed,"vaccine_derived_immunity", yaxis_title ="incidence")

plot_simulations(ship_log_completed,"R0", yaxis_title ="cumulative_incidence")
plot_simulations(ship_log_completed,"vaccine_delivery_start_date", yaxis_title ="cumulative_incidence")
#plot_simulations(ship_log_completed,"supply", yaxis_title ="cumulative_incidence")
plot_simulations(ship_log_completed,"infection_derived_immunity", yaxis_title ="cumulative_incidence")
plot_simulations(ship_log_completed,"rollout_modifier", yaxis_title ="cumulative_incidence")
plot_simulations(ship_log_completed,"vaccine_derived_immunity", yaxis_title ="cumulative_incidence")
#NB: caution this only is plotting the phase of delivery you are in at this cumulative incidence level, NOT NECCESSARY the difference in the effect of vaccines to essential workers vs the general public
################################################################################



### Plot to find where vaccine prioritisation strategies make a difference
to_plot_incidence <- ship_log_completed %>%
  filter(R0 %in% c(2,3,4)  &
           vaccine_delivery_start_date %in% c(50,100)) %>%
  group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
  summarise(incidence = sum(incidence), .groups = "keep") 

plot_simulations(data = to_plot_incidence,
                         var_1 = "R0",
                         var_2 = "vaccine_delivery_start_date",
                         yaxis_title = "incidence",
                         default_configuration = list(
                           R0 = 2,
                           vaccine_delivery_start_date = 100,
                           phase = c(
                             "all adults at the same time",
                             "essential workers",
                             "no vaccine" ),
                           supply = c(0.8),
                           infection_derived_immunity = 1,
                           rollout_modifier = 2,
                           vaccine_derived_immunity = 1
                         ),
                         colour_essential_workers_phase = 0,
                         display_var_1 = 0,
                         display_vaccine_availability = 1, 
                         display_end_of_essential_worker_delivery = 0)

#SM fig 1
plot_simulations(data = to_plot_incidence,
                         var_1 = "vaccine_delivery_start_date",
                         yaxis_title = "incidence", #cumulative_incidence, incidence
                         default_configuration = 
                           list(
                             R0 = 2,
                             vaccine_delivery_start_date = 100,
                             phase = c("older adults followed by all adults",
                                       "children before adults",
                                       "all adults at the same time",
                                       "essential workers",
                                       "no vaccine" ),
                             supply = c(0.8), #include 0 for no vaccine scenario
                             infection_derived_immunity = 1,
                             rollout_modifier = 2,
                             vaccine_derived_immunity = 1
                           ),
                         colour_essential_workers_phase = 0,
                         display_vaccine_availability = 1, 
                         display_var_1 = 0,
                         display_end_of_essential_worker_delivery = 1,
                         display_impact_heatmap = 0)

#SM fig 2
plot_simulations(data = to_plot_incidence,
                         var_1 = "R0",
                         var_2 = "rollout_modifier",
                         yaxis_title = "incidence",
                         default_configuration = list(
                           R0 = 2,
                           vaccine_delivery_start_date = 100,
                           phase = c(
                             "all adults at the same time",
                             "essential workers",
                             "no vaccine" ),
                           supply = c(0.8), #Nb: can't be 80% because then no results for rollout = 1 and 0.5, but maybe there should be....
                           infection_derived_immunity = 1,
                           rollout_modifier = 2,
                           vaccine_derived_immunity = 1
                         ),
                         colour_essential_workers_phase = 0,
                         display_vaccine_availability = 1, 
                         display_end_of_essential_worker_delivery = 0)
################################################################################



### Plot to find how prioritisation strategies are influenced by severity
this_VE <- 1
  
to_plot_severe_outcomes <- 
  project_severe_disease(
    point_estimate =  1 / 100,
    age_distribution = c("SARS","Plague","Influenza 1918"),
    VE = this_VE,
    comorb_increased_risk = 1,
    this_incidence_log_tidy = ship_log_completed,
    this_pop = loaded_setting_characteristics$population_by_comorbidity
  ) %>%
  filter(R0 %in% c(2,3,4)  &
           vaccine_delivery_start_date %in% c(50,100)) %>%
  group_by(pathogen,time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
  summarise(incidence = sum(incidence_severe_disease), .groups = "keep") 

plot_simulations(data = to_plot_severe_outcomes,
                         var_1 = "pathogen",
                         var_2 = NA,
                         yaxis_title = "incidence",
                         free_yaxis = TRUE,
                         default_configuration = list(
                           R0 = 2,
                           vaccine_delivery_start_date = 100,
                           phase = c("older adults followed by all adults",
                                    "children before adults",
                                    "all adults at the same time",
                                    "essential workers",
                                    "no vaccine" ),
                           supply = c(0.8),
                           infection_derived_immunity = 1,
                           rollout_modifier = 2,
                           vaccine_derived_immunity = this_VE
                         ),
                         colour_essential_workers_phase = 0,
                         display_var_1 = 0,
                         display_vaccine_availability = 1,
                         display_end_of_essential_worker_delivery = 0)
