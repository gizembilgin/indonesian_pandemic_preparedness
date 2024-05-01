

configure_inital_state <- function(index_case_age_group = c("30 to 59"), #CHECKED: very minor difference in results, rounded number is the same
                                   #average_symptomatic_period,
                                   #average_exposed_period,
                                   age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110"),
                                   population = loaded_setting_characteristics$population) {
  
  inital_state <- crossing(population,
                           vaccination_status = c(FALSE, TRUE)) %>%
    mutate(individuals =
             case_when(vaccination_status != FALSE ~ 0, # no one is vaccinated at the initial time point
                       TRUE ~ individuals)) 
  
  index_case <- inital_state %>%
    filter(age_group %in% index_case_age_group) %>%
    mutate(index_case = individuals/sum(individuals)) %>%
    select(-individuals)
  
  # include SEIR classes
  inital_state <- crossing(class = c("S","E","I","R","Incid"), inital_state) %>%
    mutate(individuals = case_when(
      class == "S" ~ individuals,
      class %in% c("E","I","R","Incid") ~ 0 # empty rows for incidence tracker
    )) %>%
    left_join(index_case, by = c("age_group","comorbidity","vaccination_status")) %>%
    mutate(individuals = case_when(
      class == "S" & is.na(index_case) == FALSE ~ individuals - index_case,
      class == "I" & is.na(index_case) == FALSE ~ index_case,
      TRUE ~ individuals
    )) %>%
    select(-index_case)
  
  # previous configuration with with detection_prevalence
  # inital_state <- crossing(class = c("S","E","I","R","Incid"), inital_state) %>%
  #   mutate(individuals = case_when(
  #   class == "S" ~ individuals * (1-detection_prevalence),
  #   class == "E" ~ individuals * detection_prevalence * average_exposed_period/(average_exposed_period + average_symptomatic_period),
  #   class == "I" ~ individuals * detection_prevalence * average_symptomatic_period/(average_exposed_period + average_symptomatic_period),
  #   class == "Incid" ~ 0 # empty rows for incidence tracker
  #   ))

  # order correctly
  inital_state$class <- factor(inital_state$class, levels = c("S","E","I","R","Incid"))
  inital_state$age_group <- factor(inital_state$age_group, levels = age_group_labels)
  inital_state <- inital_state %>%
    arrange(class,comorbidity,vaccination_status,age_group)  
  
  if(abs(sum(round(inital_state$individuals)) - sum(population$individuals))>1) stop("configure_inital_state: inital state does not match population")
  if(sum(inital_state$individuals[inital_state$vaccination_status != FALSE]) > 0 ) stop("configure_inital_state: inital state contains vaccinated individuals")
  
  return(inital_state)
}