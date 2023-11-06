

configure_inital_state <- function(age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110"),
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
  
  if(sum(round(inital_state$individuals)) != sum(population$individuals)){stop("inital state does not match population")}
  
  return(inital_state)
}