
require(tidyverse); require(ggpubr)

load(file = "01_inputs/population_MASTER_single_age_groups.Rdata")
urbanicity <- read.csv("01_inputs/rural_urban_breakdown_from_2022_census.csv", header = TRUE)
age_group_labels <- c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
age_groups_num <- c(0,4,17,29,59,110)
name_translation <- population_MASTER_single_age_groups %>%
  select(name_indonesian,name_english) %>%
  distinct()



### CHECK imported data ########################################################
check = population_MASTER_single_age_groups %>%
  select(name_indonesian) %>%
  distinct() %>% 
  left_join(urbanicity, by = "name_indonesian") %>%
  filter(is.na(urbanicity_percentage))
if (nrow(check)>0){stop("mismatch between province names in population_MASTER and urbanicity")}
################################################################################



### IMPORT  2017 Prem et al. national contact matrix estimates 
prem_2017 <- read.csv("01_inputs/contact_matrix_prem_2017.csv",header = FALSE)
colnames(prem_2017) <- paste0("X",seq(0,(ncol(prem_2017)-1)*5,by=5))
prem_2017 <- prem_2017 %>%
  mutate(year = 2017, 
         age_of_individual = (row_number()-1)*5) %>%
  pivot_longer(cols = -c("age_of_individual","year"),
               values_to = "contacts",
               names_to = "age_of_contact") %>%
  mutate(age_of_contact = as.numeric(gsub("X","",age_of_contact)))
################################################################################



### IMPORT 2021 contact matrix for Indonesia ###################################
prem_2021_load <- function(file_name){
  load(file = paste0("01_inputs/",file_name,".Rdata"))
  workshop <- data.frame(contact_all$IDN); rm(contact_all)
  
  workshop$location <- gsub("contact_Prem_2021","",file_name)
  workshop$location <- gsub("_","",workshop$location)
  workshop$year <- 2021 
  
  workshop <- workshop %>%
    mutate(age_of_individual = (row_number()-1)*5) %>%
    pivot_longer(cols = -c("age_of_individual","year","location"),
                 values_to = "contacts",
                 names_to = "age_of_contact") %>%
    mutate(age_of_contact = as.numeric(gsub("X","",age_of_contact)),
           age_of_contact = (age_of_contact-1)*5)
  
  return(workshop)
}
prem_2021 <- data.frame()
for(this_file_name in c("contact_Prem_2021","contact_Prem_2021_urban","contact_Prem_2021_rural")){
  workshop <- prem_2021_load(this_file_name)
  prem_2021 <- rbind(prem_2021,workshop)
}
################################################################################



### COMBINE 2017 and 2021 estimates ############################################
prem <- bind_rows(prem_2017,prem_2021) %>%
  mutate(label = case_when(
    is.na(location) ~ as.character(year),
    TRUE ~ paste(year, location,sep="_")
    ))
rm(prem_2017, prem_2021_load, prem_2021,this_file_name)

baseline <- prem %>% 
  filter(year == 2021 & location == "") %>%
  group_by(age_of_individual) %>%
  summarise(daily_contact = sum(contacts))

# ggplot(prem[prem$age_of_individual>65,])+
#   geom_line(aes(x = age_of_contact, y = contacts, colour = label),linewidth = 1) + 
#   theme(legend.position = "bottom") +
#   facet_grid(age_of_individual ~.)
#CHECKED: urban > average > rural
#CHECKED: 2017 and 2021 vaguely align, but lower contact estimates in children and higher contact estimates in older adults in updated Prem et al. paper
################################################################################



## ADAPT to our model age groups ###############################################
pop_Prem <- population_MASTER_single_age_groups %>%
  filter(name_english == "Indonesia") %>%
  mutate(agegroup_PREM = cut(age_group_single,breaks = c(0,seq(4,74,by=5),110), include.lowest = T, labels = unique(prem$age_of_individual)),
         agegroup_MODEL = cut(age_group_single,breaks = age_groups_num, include.lowest = T, labels = age_group_labels)) %>%
  ungroup() %>%
  group_by(agegroup_MODEL) %>%
  mutate(model_group_percent = individuals/sum(individuals)) %>%
  ungroup() %>%
  group_by(agegroup_PREM) %>%
  mutate(prem_group_percent = individuals/sum(individuals)) %>%
  select(age_group_single,agegroup_PREM,agegroup_MODEL,model_group_percent,prem_group_percent)
sum_1 = pop_Prem %>%
  group_by(agegroup_MODEL,agegroup_PREM) %>%
  summarise(model_group_percentage = sum(model_group_percent),.groups = "keep")
sum_2 = pop_Prem %>%
  group_by(agegroup_MODEL,agegroup_PREM) %>%
  summarise(prem_group_percentage = sum(prem_group_percent),.groups = "keep") 
pop_Prem = sum_1 %>% left_join(sum_2, by = c("agegroup_MODEL", "agegroup_PREM")); rm(sum_1,sum_2)
pop_Prem$agegroup_PREM <- as.numeric(as.character(pop_Prem$agegroup_PREM)) 

#collapse columns
workshop <- prem %>% 
  rename(agegroup_PREM = age_of_contact) %>%
  left_join(pop_Prem, by = c("agegroup_PREM"),relationship = "many-to-many") %>%
  mutate(contacts = contacts * prem_group_percentage) %>%
  rename(age_of_contact = agegroup_MODEL) %>%
  group_by(year,age_of_individual,age_of_contact,location,label) %>%
  summarise(contacts = sum(contacts), .groups = "keep")
# workshop %>% 
#   filter(year == 2021 & location == "") %>%
#   group_by(age_of_individual) %>%
#   summarise(daily_contact = sum(contacts))
#CHECKED: against baseline

#collapse rows
workshop <- workshop %>% 
  rename(agegroup_PREM = age_of_individual) %>%
  left_join(pop_Prem, by = c("agegroup_PREM"),relationship = "many-to-many") %>%
  mutate(contacts = contacts * model_group_percentage ) %>%
  rename(age_of_individual = agegroup_MODEL) %>%
  group_by(year,age_of_individual,age_of_contact,location,label) %>%
  summarise(contacts = sum(contacts), .groups = "keep")
# workshop %>% 
#   filter(year == 2021 & location == "") %>%
#   group_by(age_of_individual) %>%
#   summarise(daily_contact = sum(contacts))
#CHECKED: against baseline
rm(baseline,pop_Prem)

contact_matrix_prem <- workshop
# ggplot(contact_matrix_prem)+
#   geom_point(aes(x = age_of_contact, y = contacts, colour = label)) + 
#   theme(legend.position = "bottom") +
#   facet_grid(age_of_individual ~.)
################################################################################



### CALCULATE at provincial level ##############################################
workshop <- contact_matrix_prem %>% 
  filter(year == 2021 & location %in% c("rural","urban")) %>%
  ungroup() %>%
  select(-label,-year) %>%
  pivot_wider(names_from = "location",
              values_from = "contacts")

contact_matrix_MASTER <- crossing(age_of_individual = age_group_labels,
                                    age_of_contact = age_group_labels,
                                    name_indonesian = unique(urbanicity$name_indonesian)) %>%
  left_join(workshop, by = c("age_of_individual","age_of_contact")) %>%
  left_join(urbanicity, by = "name_indonesian") %>%
  mutate(contacts = urban * urbanicity_percentage + rural * (1-urbanicity_percentage)) %>%
  left_join(name_translation, by = "name_indonesian")%>%
  select(name_indonesian,name_english,age_of_individual,age_of_contact,contacts) 

save(contact_matrix_MASTER, file = "01_inputs/contact_matrix_MASTER.Rdata")
################################################################################



### VISUALISE VARIABILITY ######################################################
to_plot <- contact_matrix_MASTER %>% 
  group_by(name_indonesian,age_of_individual) %>%
  summarise(contacts = sum(contacts)) %>%
  arrange(age_of_individual)
to_plot$age_of_individual = factor(to_plot$age_of_individual,levels = age_group_labels)

#add pure urban and rural
workshop <- contact_matrix_prem %>% 
  filter(year == 2021 & location %in% c("rural","urban")) %>%
  ungroup() %>%
  select(-label,-year) %>%
  group_by(age_of_individual,location) %>%
  summarise(contacts = sum(contacts)) %>%
  rename(name_indonesian = location) %>%
  mutate(name_indonesian = case_when(
    name_indonesian == "rural" ~ "Indonesia (rural)",
    name_indonesian == "urban" ~ "Indonesia (urban)"
  ))

ggplot() + 
  geom_point(data = to_plot[to_plot$name_indonesian != "Indonesia",], aes(x=age_of_individual, y = contacts)) +
  geom_point(data = to_plot[to_plot$name_indonesian == "Indonesia",], aes(x=age_of_individual,y=contacts,color=as.factor(name_indonesian)),size=4,shape="cross") +
  geom_point(data = workshop, aes(x=age_of_individual,y=contacts, color = as.factor(name_indonesian)),size=4,shape="cross") +
  ylab("daily contacts") +
  xlab("") +
  theme_bw()  +
  theme(legend.position="bottom") +
  labs(color="") +
  ylim(0,max(to_plot$contacts))
################################################################################


rm(contact_matrix_prem,to_plot,check,workshop,urbanicity,prem,name_translation)