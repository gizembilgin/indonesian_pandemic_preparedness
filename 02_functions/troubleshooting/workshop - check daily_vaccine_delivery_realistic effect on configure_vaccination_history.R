the_collector <- data.frame()

for (this_scenario in c(TRUE,FALSE)){
  vaccination_strategies$daily_vaccine_delivery_realistic = this_scenario
  workshop <- configure_vaccination_history(LIST_vaccination_strategies = vaccination_strategies)
  vaccination_history <- workshop$vaccination_history %>% mutate(daily_vaccine_delivery_realistic = this_scenario)
  the_collector = rbind(the_collector, vaccination_history)
}
the_collector <- the_collector %>% 
  group_by(time,phase,supply,daily_vaccine_delivery_realistic) %>%
  summarise(doses_delivered = sum(doses_delivered), .groups = "keep")
