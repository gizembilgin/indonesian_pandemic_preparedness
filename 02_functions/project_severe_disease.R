
### This function projects from incidence -> severe disease
### The severity of this pathogen is configured by considering a population-level severity rate and age distribution of severity
### These incidence of severe disease is modified by age, vaccination status, and comorbidity
### The setting's population structure is required to scale these incidence rates appropriately to retain the desired popuation-level severity rate

project_severe_disease <- function(
    
    point_estimate = TOGGLE_severe_disease_point_estimate,
    age_distribution = TOGGLE_severe_disease_age_distribution,
    
    VE = TOGGLE_severe_disease_VE,
    comorb_increased_risk = TOGGLE_severe_disease_comorb_increased_risk, #NB: assume flat RR across age groups
    
    this_incidence_log_tidy = incidence_log_tidy,
    this_pop = loaded_setting_characteristics$population_by_comorbidity,
    this_setting = TOGGLE_setting,
    
    return_severity = FALSE #option to output severity matrix instead of severe_disease_log_tidy
    ) {
  
  if ("vaccine_derived_immunity" %in% colnames(this_incidence_log_tidy)){
    #NB: simple assumption the VE transmission == VE severe outcomes, could allow variation here instead
    this_incidence_log_tidy <- this_incidence_log_tidy %>% filter(vaccine_derived_immunity == VE)
    if (nrow(this_incidence_log_tidy) == 0) stop("project_severe_disease: this VE scenario does not exist")
  }
  
  if (is.character(age_distribution)){
    load(file = "01_inputs/age_specific_severity_MASTER.Rdata")
    if (! age_distribution %in% unique(age_specific_severity_MASTER$pathogen)) stop("project_severe_disease: you have specified the age distribution of a known pathogen, but not one included in age_specific_severity_MASTER")
    age_distribution = age_specific_severity_MASTER %>%
      filter(pathogen == age_distribution &
               (name_english == this_setting | name_indonesian == this_setting)) %>%
      select(age_group,incidence_severe_disease)
    
    #if point_estimate explicitly specified, than we are using the imported age_distribution as relative_risk not the given incidence
    if(is.na(point_estimate) == FALSE) age_distribution <- age_distribution %>% rename(relative_risk = incidence_severe_disease)
  }
  
  ### Step One: create matrix of incidence -> severe disease by age_group, vaccination_status, comorbidity
  ## (1/3) by age_group
  # calculate proportion of population within each age group
  matrix_of_severe_disease <- this_pop %>%
    group_by(age_group) %>%
    summarise(individuals = sum(individuals)) %>%
    ungroup() %>%
    mutate(pop_proportion = individuals/sum(individuals))
  
  if (is.na(point_estimate)){
    #if no point estimate specified, then use estimate of incidence_severe_disease imported
    matrix_of_severe_disease <- matrix_of_severe_disease %>%
      left_join(age_distribution, by = "age_group")
  } else{
    # modify the point_estimate of severe disease by the age-specific relative risk, then re-scale all values to retain population-level estimate
    matrix_of_severe_disease <- age_distribution %>%
      left_join(matrix_of_severe_disease, by = "age_group") %>%
      mutate(incidence_severe_disease = relative_risk * point_estimate,
             adj = incidence_severe_disease*pop_proportion,
             incidence_severe_disease = incidence_severe_disease * point_estimate/sum(adj)) %>%
      select(-adj)
    
    # #CHECK: age distribution retained
    check <- age_distribution %>%
      mutate(original_ratio = relative_risk / min(relative_risk)) %>%
      select(-relative_risk) %>%
      left_join(matrix_of_severe_disease, by = "age_group") %>%
      mutate(derived_ratio = incidence_severe_disease / min(incidence_severe_disease)) %>%
      filter(round(derived_ratio, digits = 2) != round(original_ratio, digits = 2))
    if (nrow(check)>0){stop("project_severe_disease: age distribution not applied correctly  - age distribution NOT retained")}
  }

  
  ## (2/3) by comorbidity
  # see Supplementary Material S2.4 of https://doi.org/10.1186/s12889-023-17374-0
  # need incidence rate (incidence_severe_disease), pop with comorbidity, pop without comorbitiy, RR (comorb_increased_risk)
  workshop <- this_pop %>% 
    select(age_group, comorbidity, individuals) %>%
    pivot_wider(names_from = "comorbidity", 
                values_from = "individuals",
                names_prefix = "comorb_")
  matrix_of_severe_disease <- matrix_of_severe_disease %>%
    select(age_group, individuals, incidence_severe_disease) %>% #CHECKED: individuals = comorb_0 + comorb_1
    left_join(workshop, by = "age_group") %>%
    mutate(incidence_severe_disease_0 = incidence_severe_disease*individuals/(comorb_0*(1+(comorb_increased_risk*comorb_1/comorb_0))),
           incidence_severe_disease_1 = incidence_severe_disease*individuals/(comorb_1*(1+comorb_0/(comorb_1*comorb_increased_risk))))%>%
    mutate(incidence_severe_disease_1 = case_when(
      is.nan(incidence_severe_disease_1) ~ incidence_severe_disease_0, #is NaN because no comorb in age group 0 to 4, but retain for checks
      TRUE ~ incidence_severe_disease_1
      ))
  #wrangle to tidy structure
  matrix_of_severe_disease <- matrix_of_severe_disease %>%
    select(age_group,incidence_severe_disease_0,incidence_severe_disease_1) %>%
    pivot_longer(cols = c("incidence_severe_disease_0", "incidence_severe_disease_1"),
                 names_to = "comorbidity",
                 values_to = "incidence_severe_disease",
                 names_prefix = "incidence_severe_disease_") %>%
    mutate(comorbidity = as.numeric(comorbidity)) 
  
  # #CHECK: point estimate retained. pop-level estimate = sum over age and comorb(this estimate * prop of population in this group)
  check <- matrix_of_severe_disease %>%
    left_join(this_pop,by = join_by(age_group, comorbidity)) %>%
    mutate(interim = incidence_severe_disease * individuals/sum(individuals))
  check <- sum(check$interim)
  if (round(check, digits = 6) != point_estimate & is.na(point_estimate) == FALSE ) stop("project_severe_disease: age or comorbidity distribution not applied correctly  - point estimate NOT retained")

  
  ## (3/3) by vaccination_status
  matrix_of_severe_disease <- crossing(matrix_of_severe_disease,
                                       vaccination_status = c(0,1)) %>%
    mutate(incidence_severe_disease = case_when(
      vaccination_status == 0 ~ incidence_severe_disease,
      vaccination_status == 1 ~ incidence_severe_disease * (1-VE)
    ))
  matrix_of_severe_disease$age_group <- factor(matrix_of_severe_disease$age_group, levels = levels(this_pop$age_group))
  
  if(return_severity) return(matrix_of_severe_disease) #for checking purposes
  #_____________________________________________________________________________
  
  
  
  
  ### Step Two: apply matrix_of_severe_disease to this_incidence_log_tidy
  severe_disease_log_tidy <- this_incidence_log_tidy %>%
    left_join(matrix_of_severe_disease, by = c("age_group","comorbidity","vaccination_status")) %>%
    mutate(incidence_severe_disease = incidence * incidence_severe_disease) 
  #_____________________________________________________________________________
  
  
  return(severe_disease_log_tidy)
  
}
