
### This function projects from incidence -> severe disease
### The severity of this pathogen is configured by considering a population-level severity rate and age distribution of severity
### These incidence of severe disease is modified by age, vaccination status, and comorbidity
### The setting's population structure is required to scale these incidence rates appropriately to retain the desired popuation-level severity rate

project_severe_disease <- function(data,
                                   TOGGLES_project_severe_disease = 
                                     list(
                                       point_estimate = TOGGLES_project_severe_disease$point_estimate,
                                       age_distribution = TOGGLES_project_severe_disease$age_distribution,
                                       
                                       VE_severe_disease = TOGGLES_project_severe_disease$VE_severe_disease,
                                       comorb_increased_risk = TOGGLES_project_severe_disease$comorb_increased_risk
                                       #NB: assume flat RR across age groups
                                       
                                     ),
                                   
                                   this_pop = loaded_setting_characteristics$population_by_comorbidity,
                                   this_setting = TOGGLE_setting,
                                   
                                   return_severity = FALSE #option to output severity matrix instead of severe_disease_log_tidy)
) {
  
  
  ### unfurl TOGGLES_project_severe_disease
  point_estimate = TOGGLES_project_severe_disease$point_estimate
  age_distribution = TOGGLES_project_severe_disease$age_distribution
  VE_severe_disease = TOGGLES_project_severe_disease$VE_severe_disease
  comorb_increased_risk = TOGGLES_project_severe_disease$comorb_increased_risk
  
  
  ### Load age-specific severity of specified pathogen
  if (is.character(age_distribution)){
    load(file =  paste0(gsub("/04_shiny","",getwd()),"/01_inputs/age_specific_severity_MASTER.Rdata"))
    if (length(unique(age_specific_severity_MASTER$pathogen)[unique(age_specific_severity_MASTER$pathogen) %in% age_distribution]) == 0){
      stop("project_severe_disease: you have specified the age distribution of a known pathogen, but not one included in pathogen in age_specific_severity_MASTER")
    } 
    age_distribution = age_specific_severity_MASTER %>%
      filter(pathogen %in% age_distribution &
               (name_english == this_setting | name_indonesian == this_setting)) %>%
      select(pathogen,age_group,case_fatality_rate) %>% 
      group_by(pathogen)
    
    #if point_estimate explicitly specified, than we are using the imported age_distribution as relative_risk not the given incidence
    if(is.na(point_estimate) == FALSE) age_distribution <- age_distribution %>% rename(relative_risk = case_fatality_rate)
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
    #if no point estimate specified, then use estimate of case_fatality_rate imported
    matrix_of_severe_disease <- matrix_of_severe_disease %>%
      left_join(age_distribution, by = "age_group")
  } else{
    # modify the point_estimate of severe disease by the age-specific relative risk, then re-scale all values to retain population-level estimate
    matrix_of_severe_disease <- age_distribution %>%
      left_join(matrix_of_severe_disease, by = "age_group") %>%
      mutate(case_fatality_rate = relative_risk * point_estimate,
             adj = case_fatality_rate*pop_proportion,
             case_fatality_rate = case_fatality_rate * point_estimate/sum(adj)) %>%
      select(-adj)
    
    # #CHECK: age distribution retained
    join_by_list = "age_group"
    if("pathogen" %in% colnames(age_distribution)) join_by_list = c(join_by_list, "pathogen")
    check <- age_distribution %>%
      mutate(original_ratio = relative_risk / min(relative_risk)) %>%
      select(-relative_risk) %>%
      left_join(matrix_of_severe_disease, join_by_list) %>%
      mutate(derived_ratio = case_fatality_rate / min(case_fatality_rate)) %>%
      filter(round(derived_ratio, digits = 2) != round(original_ratio, digits = 2))
    if (nrow(check)>0){stop("project_severe_disease: age distribution not applied correctly  - age distribution NOT retained")}
  }

  
  ## (2/3) by comorbidity
  # see Supplementary Material S2.4 of https://doi.org/10.1186/s12889-023-17374-0
  # need incidence rate (case_fatality_rate), pop with comorbidity, pop without comorbitiy, RR (comorb_increased_risk)
  workshop <- this_pop %>% 
    select(age_group, comorbidity, individuals) %>%
    pivot_wider(names_from = "comorbidity", 
                values_from = "individuals",
                names_prefix = "comorb_")
  matrix_of_severe_disease <- matrix_of_severe_disease %>%
    select(-pop_proportion) %>% #CHECKED: individuals = comorb_0 + comorb_1
    left_join(workshop, by = "age_group") %>%
    mutate(case_fatality_rate_0 = case_fatality_rate*individuals/(comorb_0*(1+(comorb_increased_risk*comorb_1/comorb_0))),
           case_fatality_rate_1 = case_fatality_rate*individuals/(comorb_1*(1+comorb_0/(comorb_1*comorb_increased_risk))))%>%
    mutate(case_fatality_rate_1 = case_when(
      is.nan(case_fatality_rate_1) ~ case_fatality_rate_0, #is NaN because no comorb in age group 0 to 4, but retain for checks
      TRUE ~ case_fatality_rate_1
      ))
  #wrangle to tidy structure
  matrix_of_severe_disease <- matrix_of_severe_disease %>%
    select(-individuals,-case_fatality_rate,-comorb_0,-comorb_1) %>%
    pivot_longer(cols = c("case_fatality_rate_0", "case_fatality_rate_1"),
                 names_to = "comorbidity",
                 values_to = "case_fatality_rate",
                 names_prefix = "case_fatality_rate_") %>%
    mutate(comorbidity = as.numeric(comorbidity)) 
  
  # #CHECK: point estimate retained. pop-level estimate = sum over age and comorb(this estimate * prop of population in this group)
  check <- matrix_of_severe_disease %>%
    left_join(this_pop,by = join_by(age_group, comorbidity)) %>%
    mutate(interim = case_fatality_rate * individuals/sum(individuals))
  if("pathogen" %in% names(check)) check_multiplier = length(unique(check$pathogen)) else check_multiplier = 1
  check <- sum(check$interim) / check_multiplier
  if (round(check, digits = 6) != point_estimate & is.na(point_estimate) == FALSE ) stop("project_severe_disease: age or comorbidity distribution not applied correctly  - point estimate NOT retained")

  
  ## (3/3) by vaccination_status
  workshop = data.frame()
  for (this_VE_severe_disease in VE_severe_disease){ #allowing multiple values of VE_severe_disease
    this_matrix_of_severe_disease <- crossing(matrix_of_severe_disease,
                                         vaccination_status = c(0,1)) %>%
      mutate(
        case_fatality_rate = case_when(
          vaccination_status == 0 ~ case_fatality_rate,
          vaccination_status == 1 ~ case_fatality_rate * (1 - this_VE_severe_disease)
        ), 
        VE_severe_disease = this_VE_severe_disease
      )
    workshop = rbind(workshop,this_matrix_of_severe_disease)
  }
  matrix_of_severe_disease <- workshop
  
  #put back in levels for age_group for plotting
  matrix_of_severe_disease$age_group <- factor(matrix_of_severe_disease$age_group, levels = levels(this_pop$age_group))
  
  if(return_severity) return(matrix_of_severe_disease) #for checking purposes
  #_____________________________________________________________________________
  
  
  
  
  ### Step Two: apply matrix_of_severe_disease to data
  severe_disease_log_tidy <- data %>%
    left_join(matrix_of_severe_disease, by = c("age_group","comorbidity","vaccination_status"),
              relationship = "many-to-many") %>% # if multiple pathogens
    mutate(deaths = incidence * case_fatality_rate) %>%
    select(-case_fatality_rate)
  #_____________________________________________________________________________
  
  
  return(severe_disease_log_tidy)
  
}
