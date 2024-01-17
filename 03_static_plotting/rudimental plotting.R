#This plotting script is designed for one run of the command_deck

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