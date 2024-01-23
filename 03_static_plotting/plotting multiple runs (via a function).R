#Functional plotting for multiple runs of the command_deck (e.g., fleet_admiral)

### Set default values of model configuration parameters
default_configuration = 
  list(
    R0 = 2,
    vaccine_delivery_start_date = 100,
    supply = c(0,0.2), #include 0 for no vaccine scenario
    infection_derived_immunity = 1,
    rollout_modifier = 1,
    vaccine_derived_immunity = 1
  )
#____________________________________________________________


### Apply configuration to subset data to desired scenario
configuration_filter <- function(data,configuration){
  for (i in 1:length(configuration)){
    data = data %>% filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]])
  }
  return(data)
}
#____________________________________________________________


### Function to plot influence of one variable at a time 
multiscenario_facet_plot <- function(data, # expects fleet_admiral:ship_log_completed.Rdata
                                     this_var, #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
                                     yaxis_title, #options: incidence, cumulative_incidence, cumulative_incidence_averted
                                     display_impact_heatmap = 1, #options: 0 (no), 1 (yes)
                                     display_essential_workers_phase = 1 #options: 0 (no), 1 (yes)
                                     ){
  
  # subset data to this_configuration
  this_configuration <- default_configuration[! names(default_configuration) %in% {{this_var}}]
  to_plot <-  configuration_filter(data,this_configuration)
  to_plot$phase <- factor(to_plot$phase, levels = c("older adults followed by all adults",
                                                    "children before adults",
                                                    "all adults at the same time",
                                                    "essential workers" ,
                                                    "no vaccine" ))  
  # make incidence vs time plot
  if (yaxis_title == "incidence"){
    
    if (display_essential_workers_phase == 1){
      left_plot <- to_plot %>% 
        filter(flag_reconstructed == 0)
    } else if (display_essential_workers_phase == 0){
      left_plot <- to_plot %>%
        filter(! phase %in% c("essential workers"))
    }
    
    left_plot <- ggplot(left_plot) +
      geom_line(aes(x=time,y=incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      guides(color = guide_legend(nrow = 2)) +
      theme(legend.position="bottom") +
      facet_grid(.data[[this_var]] ~.)+
      labs(title = this_var)  
  } 

  # manipulate data to cumulative data set - NB: can move to run_disease_model to save time for Shiny
  to_plot <- to_plot %>%
    group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
    arrange(time) %>%
    mutate(cumulative_incidence = cumsum(incidence))
  
  if (display_essential_workers_phase == 1){
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
      )) %>%
      select(-max_time, - cum_no_vax)
    
  } else if (display_essential_workers_phase == 0){
    to_plot <- to_plot %>%
      filter(! phase %in% c("essential workers"))
  }
  
  # make cumulative_incidence vs time plot
  if (yaxis_title == "cumulative_incidence"){
    left_plot <- ggplot(to_plot) + 
      geom_line(aes(x=time,y=cumulative_incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "")+
      ylab("cumulative incidence") + 
      guides(color = guide_legend(nrow = 2)) +
      theme(legend.position="bottom") +
      facet_grid(.data[[this_var]] ~.)+
      labs(title = this_var)
    
  }
  
  # manipulate data to cumulative incidence averted data set 
  workshop = to_plot %>%
    filter(phase == "no vaccine" & supply == 0) %>%
    ungroup() %>%
    select(-phase,-supply,-flag_reconstructed,-incidence) %>%
    rename(baseline = cumulative_incidence)
  to_plot <- to_plot %>%
    filter(phase != "no vaccine") %>%
    left_join(workshop, relationship = "many-to-many", by = join_by(time, setting, 
              vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity)) %>%
    mutate(vaccine_effect = baseline - cumulative_incidence)
  
  # make cumulative_incidence_averted vs time plot
  if (yaxis_title == "cumulative_incidence_averted"){
    to_plot$phase <- factor(to_plot$phase, levels = c("no vaccine" , 
                                                      "essential workers" ,
                                                      "older adults followed by all adults",
                                                      "all adults at the same time",
                                                      "children before adults"))
    left_plot <- ggplot(to_plot) + 
      geom_line(aes(x=time,y=vaccine_effect,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      ylab("cumulative cases averted by vaccine") +
      xlim(0,time_horizon) + 
      guides(color = guide_legend(nrow = 2))+
      theme(legend.position="bottom") +
      facet_grid(.data[[this_var]] ~.)+
      labs(title = this_var)
  }
  
  #uniform colour
  left_plot <- left_plot  +
    scale_colour_manual(values = 
                          c("no vaccine" = "#F8766D", 
                            "older adults followed by all adults" =  "#A3A500",
                            "essential workers" = "#00BF7D" ,
                            "all adults at the same time" = "#00B0F6",
                            "children before adults"  = "#E76BF3"))
  #NB: caution as these names are user defined in the command_deck (will be fixed in the Shiny)
  
  
  # function output
  if (display_impact_heatmap == 1){
    
    to_plot <- to_plot %>%
      filter(phase != "no vaccine" &
               time == time_horizon) %>%
      select(-time,-incidence,-cumulative_incidence) %>%
      mutate(vaccine_effect = vaccine_effect/baseline)
    
    right_plot <-  ggplot(to_plot) + 
      geom_tile(aes(x=.data[[this_var]],y=phase,fill=vaccine_effect)) +
      geom_text(aes(x=.data[[this_var]],y=phase,label = round(vaccine_effect,digits=2)), color = "black", size = 4)+ 
      scale_fill_gradientn(colours=c("white","orange","red","dark red"), limits=c(0,1)) +
      ylab("strategy")+
      theme(legend.position="bottom")
    
    ggarrange(left_plot,right_plot,nrow = 1)
    
  } else{
    left_plot  
  }

}


# this_var #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
# yaxis_title #options: incidence, cumulative_incidence, cumulative_incidence_averted
# display_impact_heatmap = 1 #options: 0 (no), 1 (yes)
# display_essential_workers_phase = 1 #options: 0 (no), 1 (yes)

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



