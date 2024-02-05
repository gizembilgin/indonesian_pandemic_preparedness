project_severe_disease <- function(
    
    point_estimate = TOGGLE_severe_disease_point_estimate,
    age_distribution = TOGGLE_severe_disease_age_distribution,
    
    VE = TOGGLE_severe_disease_VE,
    comorb_increased_risk = TOGGLE_severe_disease_comorb_increased_risk,
    
    this_incidence_log_tidy = incidence_log_tidy,
    this_pop = loaded_setting_characteristics$population_by_comorbidity
    ) {
  
  ### Step One: create matrix of incidence -> severe disease by age_group, vaccination_status, comorbidity

  # (1/3) by age_group
  matrix_of_severe_disease <- this_pop %>%
    group_by(age_group) %>%
    summarise(individuals = sum(individuals)) %>%
    ungroup() %>%
    mutate(pop_proportion = individuals/sum(individuals))
  matrix_of_severe_disease <- age_distribution %>%
    left_join(matrix_of_severe_disease, by = "age_group") %>%
    mutate(incidence_severe_disease = relative_risk * point_estimate,
           adj = incidence_severe_disease*pop_proportion,
           incidence_severe_disease = incidence_severe_disease * point_estimate/sum(adj)) %>%
    select(-adj)

  #CHECK: point estimate retained. pop-level estimate = sum(age-specific estimate * prop of population in this age)
  check <- matrix_of_severe_disease %>% 
    mutate(interim = incidence_severe_disease * pop_proportion)
  check <- sum(check$interim)
  if (round(check, digits = 6) != point_estimate){stop("project_severe_disease: age distribution not applied correctly  - point estimate NOT retained")}
  #CHECK: point estimate retained
  check <- age_distribution %>%
    mutate(original_ratio = relative_risk / min(relative_risk)) %>%
    select(-relative_risk) %>%
    left_join(matrix_of_severe_disease, by = "age_group") %>%
    mutate(derived_ratio = incidence_severe_disease / min(incidence_severe_disease)) %>%
    filter(round(derived_ratio, digits = 2) != round(original_ratio, digits =))
  if (nrow(check)>0){stop("project_severe_disease: age distribution not applied correctly  - age distribution NOT retained")}
  
  
  # (2/3) by comorbidity
  #see Supplementary Material S2.4 of https://doi.org/10.1186/s12889-023-17374-0
  
  # by vaccination_status
  matrix_of_severe_disease = this_incidence_log_tidy %>%
    ungroup() %>%
    select(age_group,vaccination_status,comorbidity) %>%
    unique() %>%
    mutate(incidence_severe_disease = case_when(
      vaccination_status == 0 ~ point_estimate,
      vaccination_status == 1 ~ point_estimate * (1-VE)
    ))
}
