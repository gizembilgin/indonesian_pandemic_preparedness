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
  
  this_configuration <- default_configuration[! names(default_configuration) %in% {{this_var}}]
  to_plot <-  configuration_filter(data,this_configuration)
    
  
  if (yaxis_title == "incidence"){
    
    if (include_cascade == 0){
      to_plot <- to_plot %>% 
        filter(flag_reconstructed == 0)
    } else if (include_cascade == 1){
      to_plot <- to_plot %>%
        filter(! phase %in% c("essential workers"))
    }
    
    ggplot(to_plot) +
      geom_line(aes(x=time,y=incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      guides(color = guide_legend(nrow = 2)) +
      theme(legend.position="bottom") +
      facet_grid(.data[[this_var]] ~.)+
      labs(title = this_var)
    
  } else if (yaxis_title %in% c("cumulative_incidence","cumulative_incidence_averted")){
    
    to_plot <- to_plot %>%
      group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
      arrange(time) %>%
      mutate(cumulative_incidence = cumsum(incidence))
    
    if (include_cascade == 0){
      #remove strategy before essential workers
      max_essential_workers_time = to_plot %>%
        filter(phase == "essential workers") %>%
        summarise(max_time = max(time), .groups = "keep")%>%
        ungroup() %>%
        select(-phase,-supply)
      cumulative_no_vax = to_plot %>%
        filter(time == vaccine_delivery_start_date - 1 & 
                 phase == "no vaccine") %>%
        summarise(cum_no_vax = cumulative_incidence, .groups = "keep") %>%
        ungroup() %>%
        select(-phase,-supply)
        
      to_plot <- to_plot %>% 
        left_join(max_essential_workers_time, by = join_by(setting, vaccine_delivery_start_date,
                                                           R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity)) %>%
        left_join(cumulative_no_vax, by = join_by(setting, vaccine_delivery_start_date,
                                                  R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity)) %>%
        filter(!(time <= max_time & ! phase %in% c("no vaccine","essential workers"))) %>%
        mutate(cumulative_incidence = case_when(
          phase == "essential workers" ~ cumulative_incidence + cum_no_vax,
          TRUE ~ cumulative_incidence
        ))
      
    } else if (include_cascade == 1){
      to_plot <- to_plot %>%
        filter(! phase %in% c("essential workers"))
    }
    
    if (yaxis_title == "cumulative_incidence"){
      ggplot(to_plot) + 
        geom_line(aes(x=time,y=cumulative_incidence,color=as.factor(phase)),linewidth = 1.25)  +
        labs(color="", linetype = "")+
        ylab("cumulative incidence") + 
        guides(color = guide_legend(nrow = 2)) +
        theme(legend.position="bottom") +
        facet_grid(.data[[this_var]] ~.)+
        labs(title = this_var)
      
    } else if (yaxis_title == "cumulative_incidence_averted"){
      workshop = to_plot %>%
        filter(phase == "no vaccine" & supply == 0) %>%
        ungroup() %>%
        select(time,cumulative_incidence) %>%
        rename(baseline = cumulative_incidence)
      to_plot <- to_plot %>%
        filter(phase != "no vaccine") %>%
        left_join(workshop, by = "time", relationship = "many-to-many") %>%
        mutate(vaccine_effect = baseline - cumulative_incidence)
      ggplot(to_plot) + 
        geom_line(aes(x=time,y=vaccine_effect,color=as.factor(phase)),linewidth = 1.25)  +
        labs(color="", linetype = "") +
        ylab("cumulative cases averted by vaccine") +
        xlim(0,time_horizon) + 
        guides(color = guide_legend(nrow = 2))+
        theme(legend.position="bottom") +
        facet_grid(.data[[this_var]] ~.)+
        labs(title = this_var)
    }

  } 

}


multiscenario_facet_plot(ship_log_completed,"R0","incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_delivery_start_date","incidence")
#multiscenario_facet_plot(ship_log,"supply","incidence")
multiscenario_facet_plot(ship_log_completed,"infection_derived_immunity","incidence")
multiscenario_facet_plot(ship_log_completed,"rollout_modifier","incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_derived_immunity","incidence")

multiscenario_facet_plot(ship_log_completed,"R0","cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_delivery_start_date","cumulative_incidence")
#multiscenario_facet_plot(ship_log,"supply","cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"infection_derived_immunity","cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"rollout_modifier","cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_derived_immunity","cumulative_incidence")
#NB: caution this only is plotting the phase of delivery you are in at this cumulative incidence level, NOT NECCESSARY the difference in the effect of vaccines to essential workers vs the general public











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