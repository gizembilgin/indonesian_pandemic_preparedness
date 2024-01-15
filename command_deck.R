### The 'command deck' runs all sub-scripts of the model


### Setup                 
################################################################################
# load libraries
require(tidyverse);require(deSolve); require(ggpubr)
options(scipen = 1000)

# load all functions
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}

# user toggles
age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")
TOGGLE_setting = "Indonesia" #options: "Indonesia" or name a province of Indonesia

#simulation configuration
TOGGLE_time_horizon = 365 #scope of analysis to one year
TOGGLE_introduction_day = 0 #NB: just change length of run if later
TOGGLE_introductions = 0.00001 #percentage of pop of introductions on day
TOGGLE_NPI = 0.5

# pathogen characteristics
TOGGLE_R0_to_fit = 2
TOGGLE_average_symptomatic_period = 7
TOGGLE_average_exposed_period = 7
TOGGLE_prevalence_symptoms = rep(0.8,rep(length(age_group_labels)))
TOGGLE_reduced_infectiousness_asymptomatic = 0.5
TOGGLE_susceptibility = rep(0.8,rep(length(age_group_labels)))
TOGGLE_average_immune_period = 365*10
TOGGLE_increased_risk = 2 #NOT CURRENTLY USED
TOGGLE_infection_derived_immunity = 1

#vaccination strategy
TOGGLE_vaccine_derived_immunity = 0.8
TOGGLE_vaccination_strategy = list(vaccine_delivery_start_date = 100, #NB: COVID-19 was closer to 365
                                   supply = c(0.1,0.2,0.3,0.4), #list all supply scenarios
                                   strategy = list( #list all strategies as individual lists (c(age groups), c(comorbidity status where 1 = has a comorbidity))
                                     list("older adults followed by all adults",
                                          list(c("60 to 110"),c(0,1)),
                                          list(c("18 to 29","30 to 59"),c(0,1))),
                                     list("all adults at the same time",
                                          list(c("18 to 29","30 to 59","60 to 110"),c(0,1))),
                                     list("children before adults", 
                                          list(c("0 to 4","5 to 17"),c(0,1)), 
                                          list(c("18 to 29","30 to 59","60 to 110"),c(0,1)))                                                     )
)
#_______________________________________________________________________________



### Run model            
################################################################################
loaded_setting_characteristics <- load_setting(this_setting = TOGGLE_setting)

inital_state <- configure_inital_state(
  introduction = TOGGLE_introductions,
  average_symptomatic_period = TOGGLE_average_symptomatic_period,
  average_exposed_period  = TOGGLE_average_exposed_period 
)

fitted_beta <- fit_beta_to_R0(
  R0_to_fit = TOGGLE_R0_to_fit,
  this_average_symptomatic_period = TOGGLE_average_symptomatic_period,
  this_prevalence_symptoms = TOGGLE_prevalence_symptoms,
  this_reduced_infectiousness_asymptomatic = TOGGLE_reduced_infectiousness_asymptomatic,
  this_susceptibility = TOGGLE_susceptibility
)

parameters = list(
  suscept = TOGGLE_susceptibility,
  beta=fitted_beta,
  NPI=TOGGLE_NPI,
  contact_matrix=loaded_setting_characteristics$contact_matrix,
  lota=TOGGLE_reduced_infectiousness_asymptomatic,
  gamma=TOGGLE_prevalence_symptoms,
  lambda=1/TOGGLE_average_exposed_period,
  delta=1/TOGGLE_average_symptomatic_period,
  omega=1/TOGGLE_average_immune_period,
  rho=TOGGLE_infection_derived_immunity,
  VE=TOGGLE_vaccine_derived_immunity,
  num_age_groups=num_age_groups
)

vaccination_history_permutations <- configure_vaccination_history(LIST_vaccination_strategies = TOGGLE_vaccination_strategy)

incidence_log_tidy<- run_disease_model(time_horizon = TOGGLE_time_horizon,
                                       vaccination_history = vaccination_history_permutations)
#_______________________________________________________________________________



### RUDIMENTARY PLOTTING #######################################################
plot_list <- list()

### PLOT daily incidence
to_plot <- incidence_log_tidy %>%
  group_by(time,phase,supply) %>%
  summarise(incidence = sum(incidence), .groups = "keep") %>%
  mutate(label = case_when(
    ! phase %in% c("no vaccine", "essential workers") ~ paste0(phase," (",supply*100,"%)"),
    TRUE ~ phase
  ))
order_labels <- c("no vaccine", "essential workers",
                  unique(to_plot$label[!to_plot$label %in% c("no vaccine", "essential workers")]))
to_plot$label <- factor(to_plot$label, levels = order_labels)
# ggplot(to_plot) + 
#   geom_line(aes(x=time,y=incidence,color=as.factor(label)),linewidth = 1.25)  +
#   labs(color="")
plot_list[[length(plot_list)+1]] <- ggplot(to_plot) + 
  geom_line(aes(x=time,y=incidence,color=as.factor(phase),linetype = as.factor(supply)),linewidth = 1.25)  +
  labs(color="", linetype = "") + 
  geom_vline(mapping = NULL, xintercept = min(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") + 
  geom_vline(mapping = NULL, xintercept = max(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") + 
  guides(color = guide_legend(nrow = 2))


### PLOT cumulative incidence
to_plot <- incidence_log_tidy %>%
  group_by(time,phase,supply) %>%
  summarise(incidence = sum(incidence), .groups = "keep") %>%
  group_by(phase,supply) %>%
  mutate(cumulative_incidence = cumsum(incidence))

#add no vax period and essential workers to all vaccination strategies
no_vax_cumulative <- unique(to_plot$cumulative_incidence[to_plot$time == min(to_plot$time[to_plot$phase == "essential workers"])-1])
essential_worker_cumulative <- max(to_plot$cumulative_incidence[to_plot$phase == "essential workers"])
to_plot <- to_plot %>%
  mutate(cumulative_incidence = case_when(
    phase == "no vaccine" ~ cumulative_incidence,
    phase == "essential workers" ~ cumulative_incidence + no_vax_cumulative,
    TRUE ~ cumulative_incidence + no_vax_cumulative + essential_worker_cumulative)
  )

#add cascade
for (this_supply in TOGGLE_vaccination_strategy$supply[TOGGLE_vaccination_strategy$supply != max(TOGGLE_vaccination_strategy$supply)]){
  
  next_supply =   TOGGLE_vaccination_strategy$supply[which(TOGGLE_vaccination_strategy$supply == this_supply)+1]
  next_supply_times = to_plot %>% filter(supply == next_supply)
  next_supply_times = next_supply_times$time
  
  cascade_contribution = to_plot %>%
    filter(supply == this_supply &
             phase != "essential workers" &
             !(time %in% next_supply_times)) %>%
    group_by(phase) %>%
    summarise(cascade = sum(incidence))
  
  workshop <- to_plot %>% 
    filter(supply > this_supply) %>%
    left_join(cascade_contribution, by = "phase") %>%
    mutate(cumulative_incidence = cumulative_incidence + cascade) %>%
    select(-cascade)
  
  to_plot <- to_plot %>%
    filter(is.na(supply) | supply <= this_supply)
  
  to_plot <- rbind(to_plot,workshop)
  
}

plot_list[[length(plot_list)+1]] <- ggplot(to_plot) + 
  geom_line(aes(x=time,y=cumulative_incidence,color=as.factor(phase),linetype = as.factor(supply)),linewidth = 1.25)  +
  labs(color="", linetype = "")+
  ylab("cumulative incidence") + 
  geom_vline(mapping = NULL, xintercept = min(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") + 
  geom_vline(mapping = NULL, xintercept = max(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") + 
  guides(color = guide_legend(nrow = 2))


### PLOT absolute effect of vaccine
workshop = to_plot %>%
  filter(phase == "no vaccine" & supply == 0) %>%
  ungroup() %>%
  select(time,cumulative_incidence) %>%
  rename(baseline = cumulative_incidence)
to_plot <- to_plot %>%
  left_join(workshop, by = "time", relationship = "many-to-many") %>%
  mutate(vaccine_effect = baseline - cumulative_incidence)
plot_list[[length(plot_list)+1]] <- ggplot(to_plot) + 
  geom_line(aes(x=time,y=vaccine_effect,color=as.factor(phase),linetype = as.factor(supply)),linewidth = 1.25)  +
  labs(color="", linetype = "") +
  ylab("cumulative cases averted by vaccine") +
  xlim(0,time_horizon) + 
  geom_vline(mapping = NULL, xintercept = min(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") + 
  geom_vline(mapping = NULL, xintercept = max(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") + 
  guides(color = guide_legend(nrow = 2))

ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],
          nrow = 3,
          common.legend = TRUE)

### TABULATE relative effect of vaccine
to_tabulate <- to_plot %>%
  filter(phase != "no vaccine" &
           time == time_horizon) %>%
  select(-time,-incidence,-cumulative_incidence) %>%
  mutate(vaccine_effect = vaccine_effect/baseline)
ggplot(to_tabulate) + 
  geom_tile(aes(x=supply,y=phase,fill=vaccine_effect)) +
  geom_text(aes(x=supply,y=phase,label = round(vaccine_effect,digits=2)), color = "black", size = 4)+ 
  scale_fill_gradientn(colours=c("white","orange","red","dark red"), limits=c(0,1)) +
  ylab("strategy")
#_______________________________________________________________________________