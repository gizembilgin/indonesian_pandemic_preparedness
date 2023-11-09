

configure_inital_state <- function(introduction,
                                   average_symptomatic_period,
                                   average_exposed_period,
                                   age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110"),
                                   population = loaded_setting_characteristics$population,
                                   comorbidities = loaded_setting_characteristics$comorbidities) {
  
  inital_state <- crossing(age_group = age_group_labels,
                           vaccination_status = c(0,1),
                           comorbidity = c(0,1)) %>%
    left_join(population,by = "age_group") %>%
    left_join(comorbidities, by = "age_group") %>%
    mutate(
      individuals =
        case_when(
          vaccination_status == 1 ~ 0, # assume no one vaccinated at the initial time point
          comorbidity == 0 ~ (1 - proportion) * individuals,
          comorbidity == 1 ~ proportion * individuals
        )
    ) %>%
    select(-proportion)
  
  #include SEIR classes
  inital_state <- crossing(class = c("S","E","I","R"), inital_state) %>%
    mutate(individuals = case_when(
      class == "S" ~ individuals * (1-introduction),
      class == "E" ~ individuals * introduction * average_exposed_period/(average_exposed_period + average_symptomatic_period),
      class == "I" ~ individuals * introduction * average_symptomatic_period/(average_exposed_period + average_symptomatic_period),
      class == "R" ~ 0
    ))

  # order correctly
  inital_state$class <- factor(inital_state$class, levels = c("S","E","I","R"))
  inital_state$age_group <- factor(inital_state$age_group, levels = age_group_labels)
  inital_state <- inital_state %>%
    arrange(class,comorbidity,vaccination_status,age_group)  
  
  if(abs(sum(round(inital_state$individuals)) - sum(population$individuals))>1){stop("inital state does not match population")}
  if(sum(inital_state$individuals[inital_state$vaccination_status != 0]) > 0 ){stop("inital state contains vaccinated individuals")}
  
  return(inital_state)
}