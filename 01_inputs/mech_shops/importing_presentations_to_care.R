load(file = paste0(gsub("indonesian_pandemic_preparedness","data",getwd()),"/x_results/presentations_to_care.Rdata"))
load(file = "01_inputs/population_MASTER_single_age_groups.Rdata")

age_group_labels <- c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
age_groups_num <- c(0,4,17,29,59,110)
underlying_age_grouping <- c(0,9,19,29,39,49,59,69,79,110)

presentations_to_care <- presentations_to_care %>% rename(agegroup_RAW = age)

pop_RAW =  population_MASTER_single_age_groups %>%
  filter(name_english == "Indonesia") %>%
  select(-name_english,-name_indonesian) %>%
  mutate(agegroup_RAW = cut(age_group_single,breaks = underlying_age_grouping, include.lowest = T, labels = unique(presentations_to_care$agegroup_RAW)),
         agegroup_MODEL = cut(age_group_single,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = individuals/sum(individuals)) 

presentations_to_care = pop_RAW %>% 
  left_join(presentations_to_care, by = c("agegroup_RAW")) %>% 
  mutate(interim = model_group_percent * percentage) %>%
  group_by(agegroup_MODEL) %>%
  summarise(proportion = sum(interim)) %>%
  rename(age_group = agegroup_MODEL)

save(presentations_to_care, file = "01_inputs/presentations_to_care.Rdata")

ggplot(presentations_to_care) + 
  geom_col(aes(age_group,proportion))

presentations_to_care %>%
  left_join(population, by = "age_group") %>%
  mutate(interim = individuals/sum(individuals),
         interim = proportion * interim) %>%
  summarise(presentation_ratio = sum(interim)) %>%
  arrange(presentation_ratio)
#0.385
rm(presentations_to_care)
