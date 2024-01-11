
### 11/01/2024 model doesn't run when vaccine_delivery_start_date = 50

# next_state[round(next_state$individuals)<0,]
# class age_group comorbidity vaccination_status individuals
# 15     S 60 to 110           1                  0   -3088.644
# 75     R 60 to 110           1                  0   -2799.546

# > todays_vaccinations
# age_group comorbidity doses_delivered
#   1 18 to 29            0          65926.
# 2 18 to 29            1          46393.
# 3 30 to 59            0          89304.
# 4 30 to 59            1         141398.
# 5 60 to 110           0          18653.
# 6 60 to 110           1          78759.

# this_phase
# [1] "older adults followed by all adults"
# > unique(vaccination_history$phase)
# [1] "essential_workers"                   "older adults followed by all adults" "all adults at the same time"        
# [4] "children before adults"   
#i.e., first vaccination strategy

# this_time
# [1] 125
# min(vaccination_history$time[vaccination_history$phase != "essential_workers"])
# [1] 97
#i.e., much after delivery to essential workers

vaccination_history %>%
  filter(phase %in% c(this_phase,"essential_workers") & #include essential workers always to capture day of concurrent delivery with others
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
  filter(phase %in% c(this_phase,"essential_workers") & #include essential workers always to capture day of concurrent delivery with others
           (supply == this_supply | is.na(supply))) %>%
  filter(age_group == "60 to 110") %>%
  group_by(age_group,comorbidity) %>%
  summarise(doses_delivered = sum(doses_delivered))
# age_group comorbidity doses_delivered
# <chr>           <dbl>           <dbl>
#   1 60 to 110           0         527303.
# 2 60 to 110           1        2226392.

next_state %>% 
  filter(age_group == "60 to 110") %>%
  group_by(comorbidity,vaccination_status) %>%
  summarise(individuals = sum(individuals))
# comorbidity vaccination_status individuals
# 1           0                  0     281152.
# 2           0                  1    5087232.
# 3           1                  0      -5888.
# 4           1                  1   21479426.