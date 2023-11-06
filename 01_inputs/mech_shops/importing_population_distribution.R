
require(tidyverse); require(stringr); require(ggpubr)

age_group_labels <- c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
age_groups_num <- c(0,4,17,29,59,110)


### IMPORT census data
population_MASTER <- read.csv("01_inputs/population_from_2022_census.csv",
                              header = TRUE)


### CREATE 'tidy' data structure by pivoting into an age_group column
population_MASTER <- population_MASTER %>% 
  pivot_longer(cols = c(4:ncol(population_MASTER)),
               names_to = "age_group_raw",
               values_to = "individuals")
population_MASTER$age_group_raw <- gsub("X","",population_MASTER$age_group_raw)
population_MASTER$age_group_raw <- gsub(".to."," to ",population_MASTER$age_group_raw)


### CHECK sex == 'all' <-> sex %in% c(female,male)
workshop <- population_MASTER %>%
  pivot_wider(names_from = "sex",
              values_from = "individuals") %>%
  filter(all != female + male)
if(nrow(workshop)>0){stop("male + female population != total population column in population_MASTER")}


### SELECT start and end of age group
population_MASTER <- population_MASTER %>%
  mutate(
    age_group_start = word(age_group_raw, 1, sep = "to"),
    age_group_end = word(age_group_raw, 2, sep = "to"),
    
    age_group_start = case_when(age_group_raw == "total" ~ "0",
                                TRUE ~ age_group_start),
    age_group_end = case_when(age_group_raw == "total" ~ "110",
                              TRUE ~ age_group_end)
  )
population_MASTER$age_group_start <- as.numeric(population_MASTER$age_group_start)
population_MASTER$age_group_end <- as.numeric(population_MASTER$age_group_end)


### CHECK that sum (age_group) == total 
workshop <- population_MASTER %>%
  mutate(check_grouping = 
           case_when(
             age_group_raw == "total" ~ "total",
             TRUE ~ "age_group_level_summed"
           )) %>%
  group_by(check_grouping) %>%
  summarise(individuals = sum(individuals))
if (length(unique(workshop$individuals)) != 1){stop("The total number of individuals by province does not align with the total for Indonesia")}
#remove "total" age_group as we will always want age partitioned data
population_MASTER <- population_MASTER %>%
  filter(age_group_raw != "total")

population_MASTER_five_year_age_groups <- population_MASTER %>% 
  ungroup() %>% 
  filter(sex == "all") %>%
  select(-age_group_raw,-sex)
save(population_MASTER_five_year_age_groups, file = "01_inputs/population_MASTER_five_year_age_groups.Rdata")

### CHECK Indonesia == sum(provinces) and ~ wiki population size
workshop <- population_MASTER %>%
  mutate(check_grouping = 
           case_when(
             name_english == "Indonesia" ~ "Indonesia",
             TRUE ~ "province_level_summed"
           )) %>%
  group_by(check_grouping,sex) %>%
  summarise(individuals = sum(individuals))
if (length(unique(workshop$individuals)) != length(unique(workshop$sex))){stop("The total number of individuals by province does not align with the total for Indonesia")}
baseline = sum(workshop$individuals)
#CHECKED: population ~ 270 million
to_plot_census <- population_MASTER %>% filter(sex == "all")


### ASSUME uniform distribution within reported age bands
workshop <- data.frame( age_group_single = seq(0,110)) %>%
  mutate(age_group_raw = cut(age_group_single,
                            breaks =  c(0,unique(population_MASTER$age_group_end)),
                            include.lowest = TRUE,
                            labels = unique(population_MASTER$age_group_raw)))
population_MASTER <- population_MASTER %>% 
  left_join(workshop,relationship = "many-to-many") %>%
  ungroup() %>%
  group_by(name_english,name_indonesian,sex,age_group_raw) %>%
  mutate(individuals = individuals/n()) %>%
  select(name_english,name_indonesian,sex,age_group_single,individuals)
if (sum(population_MASTER$individuals) != baseline){stop("Population size changed when moved to single age bands")}

population_MASTER_single_age_groups <- population_MASTER %>% 
  ungroup() %>% 
  filter(sex == "all") %>%
  select(-age_group_raw,-sex)
save(population_MASTER_single_age_groups, file = "01_inputs/population_MASTER_single_age_groups.Rdata")


### APPLY new model age groups
population_MASTER <- population_MASTER %>%
  mutate(age_group = cut(age_group_single,
                             breaks =  age_groups_num,
                             include.lowest = TRUE,
                             labels = age_group_labels)) %>%
  group_by(name_english,name_indonesian,sex,age_group) %>%
  summarise(individuals = sum(individuals), .groups = "keep") %>%
  ungroup()
if (sum(population_MASTER$individuals) != baseline){stop("Population size changed when moved to model age bands")}


### SAVE resultant population_file
population_MASTER_by_sex <- population_MASTER %>% filter(sex %in% c("female","male"))
save(population_MASTER_by_sex, file = "01_inputs/population_MASTER_by_sex.Rdata")
     
population_MASTER <- population_MASTER %>% filter(sex == "all") %>% select(-sex)
save(population_MASTER, file = "01_inputs/population_MASTER.Rdata")


### VISUALISE differences in provinces
provinces_to_vis = unique(population_MASTER$name_english[population_MASTER$name_english != "Indonesia"])
to_plot <- population_MASTER %>%
  filter(name_english %in% provinces_to_vis) %>%
  group_by(name_english) %>%
  mutate(percentage = individuals/sum(individuals))
plot_1 <- ggplot(to_plot) + geom_point(aes(x=age_group,y=percentage*100))+ ylab("population (%)")

to_plot <- to_plot_census %>%
  filter(name_english %in% provinces_to_vis) %>%
  group_by(name_english) %>%
  mutate(percentage = individuals/sum(individuals))
plot_2 <- ggplot(to_plot) + geom_point(aes(x=age_group_start ,y=percentage*100))+ ylab("population (%)")

provinces_to_vis = c("Central Java","Papua","West Java","West Papua")
to_plot <- population_MASTER %>%
  filter(name_english %in% provinces_to_vis) %>%
  group_by(name_english) %>%
  mutate(percentage = individuals/sum(individuals))
plot_3 <- ggplot(to_plot) + geom_point(aes(x=age_group,y=percentage*100,color=as.factor(name_english)),size = 2) + labs(position = "bottom", color = "Province") + ylab("population (%)")

to_plot <- to_plot_census %>%
  filter(name_english %in% provinces_to_vis) %>%
  group_by(name_english) %>%
  mutate(percentage = individuals/sum(individuals))
plot_4 <- ggplot(to_plot) + geom_line(aes(x=age_group_start ,y=percentage*100,color=as.factor(name_english)),linewidth = 2) + labs(position = "bottom", color = "Province") + ylab("population (%)")

ggarrange(plot_1,plot_2)
ggarrange(plot_3,plot_4, common.legend = TRUE)
