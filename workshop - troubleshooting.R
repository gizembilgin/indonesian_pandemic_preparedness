
### 11/01/2024 model doesn't run when vaccine_delivery_start_date = 50

next_state[round(next_state$individuals)<0,]
# class age_group comorbidity vaccination_status individuals
# 15     S 60 to 110           1                  0  -27648.827
# 35     E 60 to 110           1                  0   -1117.090
# 55     I 60 to 110           1                  0   -1262.656
# 75     R 60 to 110           1                  0  -12635.446

todays_vaccinations
# age_group comorbidity doses_delivered
# 1 18 to 29            0          49075.
# 2 18 to 29            1          34534.
# 3 30 to 59            0          66477.
# 4 30 to 59            1         105254.
# 5 60 to 110           0          35443.
# 6 60 to 110           1         149650.

this_phase
# [1] "older adults followed by all adults"
unique(vaccination_history$phase)
# [1] "healthcare workers"                   "older adults followed by all adults" "all adults at the same time"        
# [4] "children before adults"   
#i.e., first vaccination strategy

this_time
# [1] 125

vaccination_history %>%
  filter(phase %in% c(this_phase,"healthcare workers") & #include healthcare workers always to capture day of concurrent delivery with others
           (supply == this_supply | is.na(supply))) %>%
  filter(age_group == "60 to 110") %>%
  group_by(age_group,comorbidity) %>%
  summarise(doses_delivered = sum(doses_delivered))
# age_group comorbidity doses_delivered
# <chr>           <dbl>           <dbl>
# 60 to 110           0        4831546.
# 60 to 110           1       20399861.
loaded_setting_characteristics$population_by_comorbidity[loaded_setting_characteristics$population_by_comorbidity$age_group == "60 to 110",]
# age_group comorbidity individuals
# <chr>           <dbl>           <dbl>
# 60 to 110           0        5368384
# 60 to 110           1       21473538


vaccination_history %>%
  filter(time <= this_time) %>%
  filter(phase %in% c(this_phase,"healthcare workers") & #include healthcare workers always to capture day of concurrent delivery with others
           (supply == this_supply | is.na(supply))) %>%
  filter(age_group == "60 to 110") %>%
  group_by(age_group,comorbidity) %>%
  summarise(doses_delivered = sum(doses_delivered))
# age_group comorbidity doses_delivered
# <chr>           <dbl>           <dbl>
# 1 60 to 110           0         2703208
# 2 60 to 110           1        11413544

next_state %>% 
  filter(age_group == "60 to 110") %>%
  group_by(comorbidity,vaccination_status) %>%
  summarise(individuals = sum(individuals))
# comorbidity vaccination_status individuals
# 1           0                  0     272442.
# 2           0                  1    5095942.
# 3           1                  0     -42664.
# 4           1                  1   21516202.



vaccination_history %>%
  filter(phase %in% c("healthcare workers")) %>%
  group_by(age_group,comorbidity) %>%
  summarise(doses_delivered = sum(doses_delivered))
vaccination_history %>%
  filter(phase == this_phase & supply == this_supply & time <= this_time) %>%
  group_by(age_group,comorbidity) %>%
  summarise(doses_delivered = sum(doses_delivered))
next_state %>% 
  filter(vaccination_status == 1 &
           individuals != 0) %>%
  group_by(age_group,comorbidity) %>%
  summarise(individuals = sum(individuals), .groups = "keep")

check = vaccination_history %>% filter(phase == this_phase) %>% pivot_wider(values_from = doses_delivered, names_from = supply)
#NB: cascade not working because doses delivered per day after healthcare workers NOT the same across different vaccine supplies, even with fixed rollout capacity???? go back into configure_vaccination_history

check = vaccination_history %>%
  filter(phase == this_phase) %>%
  group_by(time,supply) %>%
  summarise(doses_delivered = sum(doses_delivered))%>% 
  pivot_wider(values_from = doses_delivered, names_from = supply)
#but daily doses delivered the same!!!  
