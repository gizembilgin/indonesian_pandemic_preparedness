
require(tidyverse); require(ggpubr)
options(scipen = 1000)

load(file = "01_inputs/population_MASTER_single_age_groups.Rdata")
workshop_RAW <- read.csv("01_inputs/age_specific_severity_known_pathogens.csv", header = TRUE)
age_group_labels <- c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
age_groups_num <- c(0,4,17,29,59,110)



### CHECK imported data ########################################################
str(workshop_RAW)
workshop_RAW %>%
  filter(value > 1 | value < 0 |
           age_start > age_end | age_start > age | age_end < age)
workshop = workshop_RAW %>%
  mutate(value = case_when(
    value > 1 ~ 1,
    TRUE ~ value
  )) %>%
  rename(case_fatality_rate = value)
################################################################################



### PLOT imported data #########################################################
ggplot(workshop,aes(x=age,y=case_fatality_rate)) + 
  #ylim(0,1)+
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~ pathogen, ncol = 3,scales = "free") 
################################################################################



### ADJUST to model age groups #################################################
workshop_model_age_groups = data.frame()
for (this_pathogen in unique(workshop$pathogen)){
  this_workshop = workshop %>%
    filter(pathogen == this_pathogen)
  
  #expand
  this_workshop_expanded = data.frame()
  for (this_row_index in 1:nrow(this_workshop)){
    this_row = this_workshop[this_row_index,]
    this_row = crossing(this_row[,c("statistic","pathogen","case_fatality_rate")],
                        age_group_single  = seq(this_row$age_start,this_row$age_end))
    this_workshop_expanded = rbind(this_workshop_expanded,this_row)
  }
  
  #join to calculate % age group this age
  this_workshop = this_workshop_expanded %>%
    left_join(population_MASTER_single_age_groups, by = join_by(age_group_single)) %>%
    mutate(age_group= cut(age_group_single,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
    group_by(statistic,pathogen,name_english,name_indonesian,age_group) %>%
    mutate(individuals = individuals/sum(individuals),
           case_fatality_rate = case_fatality_rate * individuals) %>%
    group_by(statistic,pathogen,name_english,name_indonesian,age_group) %>%
    summarise(case_fatality_rate = sum(case_fatality_rate), .groups = "keep")
  
  workshop_model_age_groups = rbind(workshop_model_age_groups,this_workshop)
}
workshop = workshop_model_age_groups
rm(this_workshop, workshop_model_age_groups, this_workshop_expanded)

ggplot(workshop[workshop$name_english == "Indonesia",],aes(x=age_group,y=case_fatality_rate)) + 
  #ylim(0,1)+
  geom_col() +
  facet_wrap(~ pathogen, ncol = 3,scales = "free")

age_specific_severity_MASTER <- workshop %>% ungroup()
save(age_specific_severity_MASTER, file = "01_inputs/age_specific_severity_MASTER.Rdata")
################################################################################


### CHECK applied ##############################################################
#Option 1: specify pt est and age dn
project_deaths(
  point_estimate =  0.05/100,
  age_distribution = data.frame(age_group = age_group_labels,
                                relative_risk = c(0.1, 0.5, 1.4, 2.7, 10.4)), 
  VE = 0,
  comorb_increased_risk = 1,
  this_incidence_log_tidy = incidence_log_tidy,
  this_pop = loaded_setting_characteristics$population,
  return_severity = TRUE
) %>%
  filter(vaccination_status == FALSE & comorbidity == FALSE) %>%
  ggplot() +
  geom_col(aes(x=age_group,y=case_fatality_rate))

#Option 2: specify pt est and select age dn
project_deaths(
  point_estimate =  0.05/100,
  age_distribution = "Plague", 
  VE = 0,
  comorb_increased_risk = 1,
  this_incidence_log_tidy = incidence_log_tidy,
  this_pop = loaded_setting_characteristics$population,
  return_severity = TRUE
) %>%
  filter(vaccination_status == FALSE & comorbidity == FALSE) %>%
  ggplot() +
  geom_col(aes(x=age_group,y=case_fatality_rate))

#Option 3: select severity profile of a known pathogen
project_deaths(
  point_estimate =  NA,
  age_distribution = "Plague", 
  VE = 0,
  comorb_increased_risk = 1,
  this_incidence_log_tidy = incidence_log_tidy,
  this_pop = loaded_setting_characteristics$population,
  return_severity = TRUE
) %>%
  filter(vaccination_status == FALSE & comorbidity == FALSE) %>%
  ggplot() +
  geom_col(aes(x=age_group,y=case_fatality_rate))

#CHECK: plot all pathogen shapes with fixed severity point estimate
project_deaths(
  TOGGLES_project_deaths = 
    list(
      point_estimate =  1 / 100,
      age_distribution = unique(age_specific_severity_MASTER$pathogen),
      VE_death = 0,
      comorb_increased_risk = 1
    ), 
  this_pop = loaded_setting_characteristics$population,
  return_severity = TRUE
) %>%
  filter(vaccination_status == FALSE & comorbidity == FALSE) %>%
  ggplot() +
  geom_col(aes(x=age_group,y=case_fatality_rate)) +
  facet_wrap(~ pathogen, ncol = 3,scales = "free")
################################################################################


### CHECK pop-level severity estimates
# age_specific_severity_MASTER %>%
#   filter(name_english == "Indonesia") %>%
#   select(-name_english,-name_indonesian,-statistic) %>%
#   left_join(population, by = "age_group") %>%
#   group_by(pathogen) %>%
#   mutate(proportion = individuals/sum(individuals),
#          interim = case_fatality_rate * proportion) %>%
#   summarise(case_fatality_rate = sum(interim)) %>%
#   arrange(case_fatality_rate)

# pathogen       case_fatality_rate
# 1 COVID-19 WT                0.0124
# 2 Influenza 1918             0.0213
# 3 Diptheria                  0.0817
# 4 SARS                       0.0915
# 5 Lassa fever                0.240 
# 6 MERS-CoV                   0.263 
# 7 JEV                        0.425 
# 8 HIV                        0.469 
# 9 Cholera                    0.470 
# 10 Tuberculosis              0.510 
# 11 Plague                    0.649 
# 12 Ebola                     0.649  