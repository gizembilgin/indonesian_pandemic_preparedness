#This plotting script is designed for multiple runs of the command_deck

default_configuration = 
  list(
    R0 = 2,
    vaccine_delivery_start_date = 100,
    supply = c(0,0.2),
    infection_derived_immunity = 1,
    rollout_modifier = 1,
    vaccine_derived_immunity = 1
  )

configuration_filter <- function(data,configuration){
  for (i in 1:length(configuration)){
    data = data %>% filter(.data[[names(configuration)[[i]]]] == configuration[[i]])
  }
  return(data)
}

multiscenario_facet_plot <- function(data,this_var){
  
  this_configuration <- default_configuration[! names(default_configuration) %in% {{this_var}}]
  
  to_plot <- configuration_filter(data,this_configuration)
  
  
  to_plot <- to_plot %>%
    group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
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
  ggplot(to_plot) +
    geom_line(aes(x=time,y=incidence,color=as.factor(phase)),linewidth = 1.25)  +
    labs(color="", linetype = "") +
    #geom_vline(mapping = NULL, xintercept = min(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") +
    #geom_vline(mapping = NULL, xintercept = max(vaccination_history_permutations$time[vaccination_history_permutations$phase == "essential workers"]), linetype="dashed") +
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position="bottom") +
    facet_grid(.data[[this_var]] ~.)
  
  #return(this_configuration)
}

multiscenario_facet_plot(ship_log,"R0")
multiscenario_facet_plot(ship_log,"vaccine_delivery_start_date")
multiscenario_facet_plot(ship_log,"supply")
multiscenario_facet_plot(ship_log,"infection_derived_immunity")
multiscenario_facet_plot(ship_log,"rollout_modifier")
multiscenario_facet_plot(ship_log,"vaccine_derived_immunity")













### PLOT cumulative incidence
to_plot <- ship_log %>%
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
#_______________________________________________________________________________