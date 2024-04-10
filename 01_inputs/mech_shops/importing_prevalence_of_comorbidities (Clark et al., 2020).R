load(file = "01_inputs/population_MASTER_single_age_groups.Rdata")
workshop <- read.csv('01_inputs/risk_group_distribution_Clarke_et_al_2020.csv')
age_group_labels <- c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
age_groups_num <- c(0,4,17,29,59,110)

setting = "Indonesia" #NB: only available for Indonesia! not at the province-level

workshop = workshop %>% 
  filter(age_group_charac != 'all ages') %>%
  filter(country_long == setting) %>%
  select(-country,-country_long) %>%
  rename(agegroup_RAW = age_group_charac,
         value = high_risk) # CHOICE between high_risk and increased_risk 

underlying_age_grouping <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,110)

pop_RAW =  population_MASTER_single_age_groups %>%
  filter(name_english == setting) %>%
  select(-name_english,-name_indonesian) %>%
  mutate(agegroup_RAW = cut(age_group_single,breaks = underlying_age_grouping, include.lowest = T, labels = unique(workshop$agegroup_RAW)),
         agegroup_MODEL = cut(age_group_single,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = individuals/sum(individuals)) 

toggle_upper_cut_off = 60  # CHOICE

comorbidities = pop_RAW %>% 
  left_join(workshop, by = c("agegroup_RAW")) %>% 
  mutate(value = case_when(
    age_group_num >= toggle_upper_cut_off ~ 1,
    TRUE ~ value
  )) %>%
  mutate(interim = model_group_percent * value) %>%
  group_by(agegroup_MODEL) %>%
  summarise(proportion = sum(interim)) %>%
  rename(age_group = agegroup_MODEL)

save(comorbidities, file = "01_inputs/comorbidities_CLARK.Rdata")
