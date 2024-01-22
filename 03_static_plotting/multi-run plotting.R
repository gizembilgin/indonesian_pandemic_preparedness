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


#____________________________________________________________

configuration_filter <- function(data,configuration){
  for (i in 1:length(configuration)){
    data = data %>% filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]])
  }
  return(data)
}


multiscenario_facet_plot <- function(data,
                                     this_var,
                                     yaxis_title,
                                     include_cascade = 0){
  
  to_plot <- data %>%
    group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
    summarise(incidence = sum(incidence), .groups = "keep") 
  if ("flag_reconstructed" %in% colnames(data) & include_cascade == 0){
    to_plot <- data %>% filter(flag_reconstructed == 0)
  } else if ("flag_reconstructed" %in% colnames(data) & include_cascade == 1){
    to_plot <- data %>%
      filter(! phase %in% c("essential workers"))
  }
  
  
  this_configuration <- default_configuration[! names(default_configuration) %in% {{this_var}}]
  to_plot <-  configuration_filter(to_plot,this_configuration)
    
  
  if (yaxis_title == "incidence"){
    
    ggplot(to_plot) +
      geom_line(aes(x=time,y=incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      guides(color = guide_legend(nrow = 2)) +
      theme(legend.position="bottom") +
      facet_grid(.data[[this_var]] ~.)+
      labs(title = this_var)
    
  } else if (yaxis_title == "cumulative_incidence"){
    
    to_plot <- to_plot %>%
      group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      mutate(cumulative_incidence = cumsum(incidence))
    
    #add no vax period and essential workers to all vaccination strategies
    no_vax_cumulative <- to_plot %>%
      filter(phase == "no vaccine" &
               time == vaccine_delivery_start_date - 1) %>%
      ungroup() %>%
      select(-time,-phase,-supply,-incidence) %>%
      rename(cumulative_no_vax = cumulative_incidence)
    
    end_of_essential_workers_phase <- to_plot %>%
      filter(phase == "essential workers") %>%
      summarise(target_time = max(time), .groups = "keep") %>%
      ungroup() %>%
      select(setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity,target_time)
    essential_workers_cumulative <- to_plot %>%
      filter(phase == "essential workers")  %>%
      left_join(end_of_essential_workers_phase, by = c("setting","vaccine_delivery_start_date","R0","infection_derived_immunity","rollout_modifier","vaccine_derived_immunity")) %>%
      filter(time == target_time) %>%
      ungroup() %>%
      select(-time,-phase,-supply,-incidence,-target_time) %>%
      rename(cumulative_essential_workers = cumulative_incidence)
      
    to_plot <- to_plot %>%
      left_join(no_vax_cumulative, by = join_by(setting, vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity)) %>%
      left_join(essential_workers_cumulative, by = join_by(setting, vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity)) %>%
      mutate(cumulative_incidence = case_when(
        phase == "no vaccine" ~ cumulative_incidence,
        phase == "essential workers" ~ cumulative_incidence + cumulative_no_vax,
        TRUE ~ cumulative_incidence + cumulative_no_vax + cumulative_essential_workers)
      )
    

    
    ggplot(to_plot) + 
      geom_line(aes(x=time,y=cumulative_incidence,color=as.factor(phase),linetype = as.factor(supply)),linewidth = 1.25)  +
      labs(color="", linetype = "")+
      ylab("cumulative incidence") + 
      guides(color = guide_legend(nrow = 2))

  }

}

multiscenario_facet_plot(ship_log,"R0","incidence")
multiscenario_facet_plot(ship_log_completed,"R0","incidence")

multiscenario_facet_plot(ship_log,"vaccine_delivery_start_date","incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_delivery_start_date","incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_delivery_start_date","incidence",include_cascade = 1)

#multiscenario_facet_plot(ship_log,"supply","incidence")
multiscenario_facet_plot(ship_log,"infection_derived_immunity","incidence")
multiscenario_facet_plot(ship_log,"rollout_modifier","incidence")
multiscenario_facet_plot(ship_log,"vaccine_derived_immunity","incidence")













### PLOT cumulative incidence



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