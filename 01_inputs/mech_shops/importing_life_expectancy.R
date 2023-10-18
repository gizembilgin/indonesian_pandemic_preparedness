
require(tidyverse); require(ggpubr)

life_expectancy_MASTER <- read.csv("01_inputs/life_expectancy_from_2022_census.csv",
                              header = TRUE)

### LOAD population_MASTER_by_sex and for this instance sum across age groups
load(file = "01_inputs/population_MASTER_by_sex.Rdata")
population_MASTER_by_sex <- population_MASTER_by_sex %>%
  group_by(name_english,name_indonesian, sex) %>%
  summarise(individuals = sum(individuals), .groups = "keep")

### FILTER to 2022
life_expectancy_MASTER <- life_expectancy_MASTER %>%
  filter(year == 2022) %>%
  select(-year)


### JOIN to population_MASTER_by_sex
life_expectancy_MASTER <- life_expectancy_MASTER %>%
  left_join(population_MASTER_by_sex,by = join_by(name_indonesian, sex))


### CALCULATE life_expectancy = prop_male * male_life_expectancy + prop_female * female_life_expectancy
life_expectancy_MASTER <- life_expectancy_MASTER %>%
  pivot_wider(names_from = sex,
              values_from = c("life_expectancy","individuals")) %>%
  mutate(total_individuals = individuals_male  + individuals_female,
         life_expectancy = life_expectancy_male * (individuals_male/total_individuals) + 
           life_expectancy_female * (individuals_female/total_individuals)) %>%
  select(name_indonesian,name_english,life_expectancy)


### VISUALISE
life_expectancy_MASTER %>% 
  filter(name_english != "Indonesia") %>%
  ggplot() + 
  geom_point(aes(x=life_expectancy, y = reorder(name_english,life_expectancy))) +
  ylab("") +
  xlab("life expectancy (years)") + 
  theme_bw() +
  geom_vline(xintercept = life_expectancy_MASTER$life_expectancy[life_expectancy_MASTER$name_english == "Indonesia"], 
             linetype = "dashed")


### SAVE
save(life_expectancy_MASTER, file = "01_inputs/life_expectancy_MASTER.Rdata")
