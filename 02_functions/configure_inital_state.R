

configure_inital_state <- function(introduction,
                                   average_symptomatic_period,
                                   average_exposed_period,
                                   age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110"),
                                   population_by_comorbidity = loaded_setting_characteristics$population_by_comorbidity) {
  
  inital_state <- crossing(population_by_comorbidity,
                           vaccination_status = c(0, 1)) %>%
    mutate(individuals =
             case_when(vaccination_status != 0 ~ 0, # no one is vaccinated at the initial time point
                       TRUE ~ individuals)) 
  
  # include SEIR classes
  inital_state <- crossing(class = c("S","E","I","R","Incid"), inital_state) %>%
    mutate(individuals = case_when(
      class == "S" ~ individuals * (1-introduction),
      class == "E" ~ individuals * introduction * average_exposed_period/(average_exposed_period + average_symptomatic_period),
      class == "I" ~ individuals * introduction * average_symptomatic_period/(average_exposed_period + average_symptomatic_period),
      class == "R" ~ 0,
      class == "Incid" ~ 0 # empty rows for incidence tracker
    ))

  # order correctly
  inital_state$class <- factor(inital_state$class, levels = c("S","E","I","R","Incid"))
  inital_state$age_group <- factor(inital_state$age_group, levels = age_group_labels)
  inital_state <- inital_state %>%
    arrange(class,comorbidity,vaccination_status,age_group)  
  
  if(abs(sum(round(inital_state$individuals)) - sum(population_by_comorbidity$individuals))>1) stop("configure_inital_state: inital state does not match population")
  if(sum(inital_state$individuals[inital_state$vaccination_status != 0]) > 0 ) stop("configure_inital_state: inital state contains vaccinated individuals")
  
  return(inital_state)
}