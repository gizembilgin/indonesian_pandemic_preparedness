
require(tidyverse); require(ggpubr)

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
  ))
################################################################################



### PLOT imported data #########################################################
ggplot(workshop,aes(x=age,y=value)) + 
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
    this_row = crossing(this_row[,c("statistic","pathogen","value")],
                        age_group_single  = seq(this_row$age_start,this_row$age_end))
    this_workshop_expanded = rbind(this_workshop_expanded,this_row)
  }
  
  #join to calculate % age group this age
  this_workshop = this_workshop_expanded %>%
    left_join(population_MASTER_single_age_groups, by = join_by(age_group_single)) %>%
    mutate(age_group= cut(age_group_single,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
    group_by(statistic,pathogen,name_english,name_indonesian,age_group) %>%
    mutate(individuals = individuals/sum(individuals),
           value = value * individuals) %>%
    group_by(statistic,pathogen,name_english,name_indonesian,age_group) %>%
    summarise(value = sum(value), .groups = "keep")
  
  workshop_model_age_groups = rbind(workshop_model_age_groups,this_workshop)
}
workshop = workshop_model_age_groups
rm(this_workshop, workshop_model_age_groups, this_workshop_expanded)

ggplot(workshop[workshop$name_english == "Indonesia",],aes(x=age_group,y=value)) + 
  #ylim(0,1)+
  geom_point() +
  facet_wrap(~ pathogen, ncol = 3,scales = "free")

age_specific_severity_MASTER <- workshop
save(age_specific_severity_MASTER, file = "01_inputs/age_specific_severity_MASTER.Rdata")
################################################################################
