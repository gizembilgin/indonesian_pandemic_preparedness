
### This function projects from incidence -> death
### The severity of this pathogen is configured by considering a population-level severity rate and age distribution of severity
### These incidence of death is modified by age, vaccination status, and comorbidity
### The setting's population structure is required to scale these incidence rates appropriately to retain the desired popuation-level severity rate

project_deaths <- function(data = data.frame(),
                                   TOGGLES_project_deaths = 
                                     list(
                                       point_estimate = TOGGLES_project_deaths$point_estimate,
                                       age_distribution = TOGGLES_project_deaths$age_distribution,
                                       
                                       VE_death = TOGGLES_project_deaths$VE_death,
                                       comorb_increased_risk = TOGGLES_project_deaths$comorb_increased_risk
                                       #NB: assume flat RR across age groups
                                       
                                     ),
                                   
                                   this_pop = loaded_setting_characteristics$population,
                                   this_setting = TOGGLE_setting,
                                   
                                   return_severity = FALSE #option to output severity matrix instead of deaths_log_tidy)
) {
  
  
  ### unfurl TOGGLES_project_deaths
  point_estimate = TOGGLES_project_deaths$point_estimate
  age_distribution = TOGGLES_project_deaths$age_distribution
  VE_death = TOGGLES_project_deaths$VE_death
  comorb_increased_risk = TOGGLES_project_deaths$comorb_increased_risk
  
  
  ### Load age-specific severity of specified pathogen
  if (is.character(age_distribution)){
    load(file =  paste0(gsub("/04_shiny","",getwd()),"/01_inputs/age_specific_severity_MASTER.Rdata"))
    if (length(unique(age_specific_severity_MASTER$pathogen)[unique(age_specific_severity_MASTER$pathogen) %in% age_distribution]) == 0){
      stop("project_deaths: you have specified the age distribution of a known pathogen, but not one included in pathogen in age_specific_severity_MASTER")
    } 
    age_distribution = age_specific_severity_MASTER %>%
      filter(pathogen %in% age_distribution &
               (name_english == this_setting | name_indonesian == this_setting)) %>%
      select(pathogen,age_group,infection_fatality_ratio) %>% 
      group_by(pathogen)
    
    #if point_estimate explicitly specified, than we are using the imported age_distribution as relative_risk not the given incidence
    if(is.na(point_estimate) == FALSE) age_distribution <- age_distribution %>% rename(relative_risk = infection_fatality_ratio)
  }
  
  
  
  ### Step One: create matrix of incidence -> death by age_group, vaccination_status, comorbidity
  ## (1/3) by age_group
  # calculate proportion of population within each age group
  matrix_of_deaths <- this_pop %>%
    group_by(age_group) %>%
    summarise(individuals = sum(individuals)) %>%
    ungroup() %>%
    mutate(pop_proportion = individuals/sum(individuals))
  
  if (is.na(point_estimate)){
    #if no point estimate specified, then use estimate of infection_fatality_ratio imported
    matrix_of_deaths <- matrix_of_deaths %>%
      left_join(age_distribution, by = "age_group")
  } else{
    # modify the point_estimate of death by the age-specific relative risk, then re-scale all values to retain population-level estimate
    matrix_of_deaths <- age_distribution %>%
      left_join(matrix_of_deaths, by = "age_group") %>%
      mutate(infection_fatality_ratio = relative_risk * point_estimate,
             adj = infection_fatality_ratio*pop_proportion,
             infection_fatality_ratio = infection_fatality_ratio * point_estimate/sum(adj)) %>%
      select(-adj)
    
    # #CHECK: age distribution retained
    join_by_list = "age_group"
    if("pathogen" %in% colnames(age_distribution)) join_by_list = c(join_by_list, "pathogen")
    check <- age_distribution %>%
      mutate(original_ratio = relative_risk / min(relative_risk)) %>%
      select(-relative_risk) %>%
      left_join(matrix_of_deaths, join_by_list) %>%
      mutate(derived_ratio = infection_fatality_ratio / min(infection_fatality_ratio)) %>%
      filter(round(derived_ratio, digits = 2) != round(original_ratio, digits = 2))
    if (nrow(check)>0){stop("project_deaths: age distribution not applied correctly  - age distribution NOT retained")}
  }

  
  ## (2/3) by comorbidity
  # see Supplementary Material S2.4 of https://doi.org/10.1186/s12889-023-17374-0
  # need incidence rate (infection_fatality_ratio), pop with comorbidity, pop without comorbitiy, RR (comorb_increased_risk)
  if (length(unique(this_pop$comorbidity))>1){
    workshop <- this_pop %>%
      select(age_group, comorbidity, individuals) %>%
      pivot_wider(names_from = "comorbidity",
                  values_from = "individuals",
                  names_prefix = "comorb_")
    matrix_of_deaths <- matrix_of_deaths %>%
      #select(-pop_proportion) %>% #CHECKED: individuals = comorb_0 + comorb_1
      left_join(workshop, by = "age_group") %>%
      mutate(infection_fatality_ratio_0 = infection_fatality_ratio*individuals/(comorb_0*(1+(comorb_increased_risk*comorb_1/comorb_0))),
             infection_fatality_ratio_1 = infection_fatality_ratio*individuals/(comorb_1*(1+comorb_0/(comorb_1*comorb_increased_risk))))%>%
      mutate(infection_fatality_ratio_1 = case_when(
        is.nan(infection_fatality_ratio_1) ~ infection_fatality_ratio_0, #is NaN because no comorb in age group 0 to 4, but retain for checks
        TRUE ~ infection_fatality_ratio_1
        ))
    #wrangle to tidy structure
    matrix_of_deaths <- matrix_of_deaths %>%
      select(-individuals,-infection_fatality_ratio,-comorb_0,-comorb_1) %>%
      pivot_longer(cols = c("infection_fatality_ratio_0", "infection_fatality_ratio_1"),
                   names_to = "comorbidity",
                   values_to = "infection_fatality_ratio",
                   names_prefix = "infection_fatality_ratio_")
  } else { # if only one
    matrix_of_deaths <- matrix_of_deaths %>%
      mutate(comorbidity = FALSE) %>%
      select(-individuals)
  }

  
  # #CHECK: point estimate retained. pop-level estimate = sum over age and comorb(this estimate * prop of population in this group)
  check <- matrix_of_deaths %>%
    left_join(this_pop,by = join_by(age_group, comorbidity)) %>%
    mutate(interim = infection_fatality_ratio * individuals/sum(individuals))
  if("pathogen" %in% names(check)) check_multiplier = length(unique(check$pathogen)) else check_multiplier = 1
  check <- sum(check$interim) / check_multiplier
  if (round(check, digits = 6) != point_estimate & is.na(point_estimate) == FALSE ) stop("project_deaths: age or comorbidity distribution not applied correctly  - point estimate NOT retained")

  
  ## (3/3) by vaccination_status
  workshop = data.frame()
  for (this_VE_death in VE_death){ #allowing multiple values of VE_death
    this_matrix_of_deaths <- crossing(matrix_of_deaths,
                                         vaccination_status = c(FALSE,TRUE)) %>%
      mutate(
        infection_fatality_ratio = case_when(
          vaccination_status == FALSE ~ infection_fatality_ratio,
          vaccination_status == TRUE ~ infection_fatality_ratio * (1 - this_VE_death)
        ), 
        VE_death = this_VE_death
      )
    workshop = rbind(workshop,this_matrix_of_deaths)
  }
  matrix_of_deaths <- workshop
  
  #put back in levels for age_group for plotting
  matrix_of_deaths$age_group <- factor(matrix_of_deaths$age_group, levels = levels(this_pop$age_group))
  
  if(return_severity) return(matrix_of_deaths) #for checking purposes
  #_____________________________________________________________________________
  
  
  
  
  ### Step Two: apply matrix_of_deaths to data
  deaths_log_tidy <- data %>%
    left_join(matrix_of_deaths, by = c("age_group","comorbidity","vaccination_status"),
              relationship = "many-to-many") %>% # if multiple pathogens
    mutate(deaths = incidence * infection_fatality_ratio) %>%
    select(-infection_fatality_ratio)
  #_____________________________________________________________________________
  
  
  return(deaths_log_tidy)
  
}
