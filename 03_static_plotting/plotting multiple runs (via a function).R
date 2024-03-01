#Functional plotting for multiple runs of the command_deck (e.g., fleet_admiral)

### Load simulation
list_poss_Rdata = list.files(
  path = "04_shiny/x_results/",
  pattern = "ship_log_completed*"
)
if (length(list_poss_Rdata) > 0) {
  list_poss_Rdata_details = double()
  for (j in 1:length(list_poss_Rdata)) {
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste0("04_shiny/x_results/", list_poss_Rdata[[j]]))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste0("04_shiny/x_results/",latest_file))
} else{
  stop("shiny: can't find underlying simulation to load!")
}


### Apply configuration to subset data to desired scenario
configuration_filter <- function(data,configuration,warning_search = 0){
  for (i in 1:length(configuration)){
    if (names(configuration)[i] == "supply"){
      data = data %>% 
        filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]] | 
                 phase %in% c("no vaccine", "essential workers"))
    } else{
      data = data %>% 
        filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]]) 
    }
    if (warning_search == 1 & nrow(data[data$supply != 0,]) == 0){
      return(names(configuration)[[i]])
    }
  }
  return(data)
}
#____________________________________________________________


### Function to plot influence of one variable at a time 
multiscenario_facet_plot <- function(data, # expects fleet_admiral:ship_log_completed.Rdata
                                     var_1, #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
                                     var_2 = NA,
                                     yaxis_title, #options: incidence, cumulative_incidence, cumulative_incidence_averted
                                     default_configuration = 
                                       list(
                                         R0 = 2,
                                         vaccine_delivery_start_date = 100,
                                         phase = c("older adults followed by all adults",
                                                   "children before adults",
                                                   "all adults at the same time",
                                                   "essential workers",
                                                   "no vaccine" ),
                                         supply = c(0.2), #include 0 for no vaccine scenario
                                         infection_derived_immunity = 1,
                                         rollout_modifier = 2,
                                         vaccine_derived_immunity = 1
                                       ),
                                     display_impact_heatmap = 1, #options: 0 (no), 1 (yes)
                                     display_var_1 = 1,
                                     colour_essential_workers_phase = 1, #options: 0 (no), 1 (yes)
                                     display_vaccine_availability = 1, #options: 0 (no), 1 (yes)
                                     display_end_of_essential_worker_delivery = 1  #options: 0 (no), 1 (yes)
                                     ){
  
  ### Subset data to this_configuration
  this_configuration <- default_configuration[! names(default_configuration) %in% c({{var_1}},{{var_2}})]
  to_plot <-  configuration_filter(data,this_configuration)
  include_strategies <- default_configuration$phase[! default_configuration$phase %in% c("essential workers","no vaccine" )]
  
  
  ### Rename phase to include var_2 in plot if relevant
  if (is.na(var_2) == FALSE){
    if (length(include_strategies)>1){
      to_plot <- to_plot %>%
        mutate(phase = case_when(
          phase %in% c("no vaccine","essential workers") ~ phase,
          TRUE ~ paste(phase,"(",var_2,.data[[var_2]],")")
        ))
    } else{
      
      if (var_2 != "vaccine_delivery_start_date"){ # general case
        to_plot <- to_plot %>%
          mutate(phase = case_when(
            phase %in% c("no vaccine","essential workers") ~ phase,
            TRUE ~ paste(var_2,.data[[var_2]])
          ))
      } else if (var_2 == "vaccine_delivery_start_date"){
        to_plot <- to_plot %>%
          mutate(phase = case_when(
            phase %in% c("no vaccine","essential workers") ~ phase,
            TRUE ~ paste("vaccine delivery starting at",.data[[var_2]],"days")
          ))
      }

      to_plot$phase <- factor(to_plot$phase, levels = c(unique(to_plot$phase[to_plot$phase != "no vaccine"]) ,
                                                            "no vaccine" ))
    }
  } else{
    to_plot$phase <- factor(to_plot$phase, levels = c("older adults followed by all adults",
                                                      "children before adults",
                                                      "all adults at the same time",
                                                      "essential workers" ,
                                                      "no vaccine" ))
  }
  
  
  ### Send error if no simulations selected
  if (nrow(to_plot[to_plot$phase != "no vaccine",]) == 0){
    return(configuration_filter(data,this_configuration,warning_search = 1))
  }
  
  
  ### Collect information on the last date of essential worker rollout in case the explicit labelling of this phase is dropped later
  vline_data2 <- to_plot %>%
    filter(phase == "essential workers") %>%
    group_by(.data[[var_1]]) %>%
    summarise(end_of_essential_workers_phase = max(time), .groups = "keep") 
  
  
  ### Making plot option 1/3: Incidence vs time
  if (yaxis_title == "incidence"){
    
    if (colour_essential_workers_phase == 1){
      to_plot_left_plot <- to_plot
    } else if (colour_essential_workers_phase == 0){
      to_plot_left_plot <- to_plot %>%
        filter(! phase %in% c("essential workers"))
    }

   left_plot <- ggplot(to_plot_left_plot) +
      geom_line(aes(x=time,y=incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      guides(color = guide_legend(nrow = 2)) +
      facet_grid(.data[[var_1]] ~.)
  } 

  
  ### Calculate cumulative_incidence
  # manipulate data to cumulative data set - NB: can move to run_disease_model to save time for Shiny
  to_plot <- to_plot %>%
    group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
    arrange(time) %>%
    mutate(cumulative_incidence = cumsum(incidence))
  
  
  ### Isolate essential worker period if colour_essential_workers_phase == 1 
  if (colour_essential_workers_phase == 1){
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
    
  } else if (colour_essential_workers_phase == 0){
    to_plot <- to_plot %>%
      filter(! phase %in% c("essential workers"))
  }
  
  
  ### Making plot option 2/3: Cumulative incidence vs time
  if (yaxis_title == "cumulative_incidence"){
    to_plot_left_plot <- to_plot
    left_plot <- ggplot(to_plot_left_plot) + 
      geom_line(aes(x=time,y=cumulative_incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "")+
      ylab("cumulative incidence") + 
      guides(color = guide_legend(nrow = 2)) +
      facet_grid(.data[[var_1]] ~.)
  }
  
  
  ### Calculate cumulative incidence averted
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
  
  
  ### Making plot option 3/3: Cumulative incidence averted vs time
  if (yaxis_title == "cumulative_incidence_averted"){
    to_plot_left_plot <- to_plot
    left_plot <- ggplot(to_plot_left_plot) + 
      geom_line(aes(x=time,y=vaccine_effect,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      ylab("cumulative cases averted by vaccine") +
      xlim(0,time_horizon) + 
      guides(color = guide_legend(nrow = 2)) +
      facet_grid(.data[[var_1]] ~.)
  }
  
  
  ### Apply plotting visuals
  #uniform colours, NB: caution as these names are user defined in the command_deck (will be fixed in the Shiny)
  defined_colour_palette <-  c("no vaccine" = "#F8766D", 
                              "older adults followed by all adults" =  "#A3A500",
                              "essential workers" = "#00BF7D" ,
                              "all adults at the same time" = "#00B0F6",
                              "children before adults"  = "#E76BF3")
  if(is.na(var_2) == FALSE){
    defined_colour_palette <- defined_colour_palette[1:length(unique(to_plot_left_plot$phase))]
    names(defined_colour_palette) <- unique(to_plot_left_plot$phase)
    
  }
  
 left_plot <- left_plot  +
    scale_colour_manual(values = defined_colour_palette) +
   theme_bw() +
   theme(legend.position="bottom")
 if (display_var_1 == 1) left_plot <- left_plot + labs(title = var_1) 
 
 ### Apply plotting options (vlines)
  #dashed line for first day of vaccine availability 
  if (display_vaccine_availability == 1){
    vline_data <- to_plot %>%
      group_by({{var_1}}) %>%
      reframe(vaccine_delivery_start_date = unique(vaccine_delivery_start_date))

    left_plot <- left_plot + geom_vline(data = vline_data, aes(xintercept = vaccine_delivery_start_date), linetype = "dashed")
    
  }
  #dashed line for last day of essential worker delivery
  if (display_end_of_essential_worker_delivery == 1){
    left_plot <- left_plot + geom_vline(data = vline_data2, aes(xintercept = end_of_essential_workers_phase), linetype = "dashed")
  }

  
  ### Make heatmap & output plot!
  if (display_impact_heatmap == 1){
    
    to_plot <- to_plot %>%
      filter(phase != "no vaccine" &
               time == time_horizon) %>%
      select(-time,-incidence,-cumulative_incidence) %>%
      mutate(vaccine_effect = vaccine_effect/baseline)
    
    if (is.na(var_2) == FALSE & length(include_strategies) == 1){
      to_plot$phase <- parse_number(as.character(to_plot$phase))
      to_plot <- to_plot %>%
        arrange(phase) %>%
        mutate(phase = as.factor(phase))
    }
    
    right_plot <-  ggplot(to_plot) + 
      geom_tile(aes(x=.data[[var_1]],y=phase,fill=vaccine_effect)) +
      geom_text(aes(x=.data[[var_1]],y=phase,label = round(vaccine_effect,digits=2)), color = "black", size = 4)+ 
      scale_fill_gradientn(colours=c("white","orange","red","dark red"), limits=c(0,1)) +
      ylab("strategy") +
      scale_x_reverse() +
      coord_flip() +
      theme_bw() +
      theme(legend.position="bottom")
    
    if (is.na(var_2) == FALSE){
      right_plot <- right_plot + 
        ylab(gsub("_"," ",var_2))
    }
    
    ggarrange(left_plot,right_plot,nrow = 1)
    
  } else{
    left_plot  
  }

}


# var_1 #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
# yaxis_title #options: incidence, cumulative_incidence, cumulative_incidence_averted
# display_impact_heatmap = 1 #options: 0 (no), 1 (yes)
# colour_essential_workers_phase = 1 
#display_vaccine_availability = 0, 
#display_end_of_essential_worker_delivery = 0

multiscenario_facet_plot(ship_log_completed,"R0", yaxis_title ="incidence",display_vaccine_availability = 1, display_end_of_essential_worker_delivery = 1)
multiscenario_facet_plot(ship_log_completed,"vaccine_delivery_start_date", yaxis_title ="incidence")
#multiscenario_facet_plot(ship_log_completed,"supply", yaxis_title ="incidence")
multiscenario_facet_plot(ship_log_completed,"infection_derived_immunity", yaxis_title ="incidence")
multiscenario_facet_plot(ship_log_completed,"rollout_modifier", yaxis_title ="incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_derived_immunity", yaxis_title ="incidence")

multiscenario_facet_plot(ship_log_completed,"R0", yaxis_title ="cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_delivery_start_date", yaxis_title ="cumulative_incidence")
#multiscenario_facet_plot(ship_log_completed,"supply", yaxis_title ="cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"infection_derived_immunity", yaxis_title ="cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"rollout_modifier", yaxis_title ="cumulative_incidence")
multiscenario_facet_plot(ship_log_completed,"vaccine_derived_immunity", yaxis_title ="cumulative_incidence")
#NB: caution this only is plotting the phase of delivery you are in at this cumulative incidence level, NOT NECCESSARY the difference in the effect of vaccines to essential workers vs the general public

to_plot <- ship_log_completed %>%
  filter(R0 %in% c(2,3,4))
multiscenario_facet_plot(data = to_plot,
                         var_1 = "R0",
                         var_2 = "vaccine_delivery_start_date",
                         yaxis_title = "incidence",
                         default_configuration = list(
                           R0 = 2,
                           vaccine_delivery_start_date = 100,
                           phase = c(
                             "all adults at the same time",
                             "essential workers",
                             "no vaccine" ),
                           supply = c(0.8),
                           infection_derived_immunity = 1,
                           rollout_modifier = 2,
                           vaccine_derived_immunity = 1
                         ),
                         colour_essential_workers_phase = 0,
                         display_var_1 = 0,
                         display_vaccine_availability = 1, 
                         display_end_of_essential_worker_delivery = 0)

#SM fig 1
multiscenario_facet_plot(data = ship_log_completed,
                         var_1 = "vaccine_delivery_start_date",
                         yaxis_title = "cumulative_incidence",
                         default_configuration = 
                           list(
                             R0 = 4,
                             vaccine_delivery_start_date = 100,
                             phase = c("older adults followed by all adults",
                                       "children before adults",
                                       "all adults at the same time",
                                       "essential workers",
                                       "no vaccine" ),
                             supply = c(0.8), #include 0 for no vaccine scenario
                             infection_derived_immunity = 1,
                             rollout_modifier = 2,
                             vaccine_derived_immunity = 1
                           ),
                         colour_essential_workers_phase = 0,
                         display_vaccine_availability = 1, 
                         display_end_of_essential_worker_delivery = 0,
                         display_impact_heatmap = 0)

#SM fig 2
multiscenario_facet_plot(data = ship_log_completed,
                         var_1 = "R0",
                         var_2 = "rollout_modifier",
                         yaxis_title = "incidence",
                         default_configuration = list(
                           R0 = 2,
                           vaccine_delivery_start_date = 100,
                           phase = c(
                             "all adults at the same time",
                             "essential workers",
                             "no vaccine" ),
                           supply = c(0.2), #Nb: can't be 80% because then no results for rollout = 1 and 0.5, but maybe there should be....
                           infection_derived_immunity = 1,
                           rollout_modifier = 2,
                           vaccine_derived_immunity = 1
                         ),
                         colour_essential_workers_phase = 0,
                         display_vaccine_availability = 1, 
                         display_end_of_essential_worker_delivery = 0)
