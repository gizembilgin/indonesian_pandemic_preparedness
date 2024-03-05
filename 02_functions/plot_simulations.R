plot_simulations <- function(data, # expects fleet_admiral:ship_log_completed.Rdata
                                     var_1, #options:vaccine_delivery_start_date, R0, infection_derived_immunity, rollout_modifier, vaccine_derived_immunity
                                     var_2 = NA,
                                     yaxis_title, #options: incidence, cumulative_incidence, cumulative_incidence_averted
                                     free_yaxis = FALSE,
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
                                     load_simulations = TRUE, #load simulations for each run
                                     display_impact_heatmap = 1, #options: 0 (no), 1 (yes)
                                     display_var_1 = 1,
                                     colour_essential_workers_phase = 1, #options: 0 (no), 1 (yes)
                                     display_vaccine_availability = 1, #options: 0 (no), 1 (yes)
                                     display_end_of_essential_worker_delivery = 1  #options: 0 (no), 1 (yes)
){
  
  ### Load simulation
  if (load_simulations == TRUE){
    # list_poss_Rdata = list.files(
    #   path = "04_shiny/x_results/",
    #   pattern = "ship_log_completed*"
    # )
    # if (length(list_poss_Rdata) > 0) {
    #   list_poss_Rdata_details = double()
    #   for (j in 1:length(list_poss_Rdata)) {
    #     list_poss_Rdata_details = rbind(list_poss_Rdata_details,
    #                                     file.info(paste0("04_shiny/x_results/", list_poss_Rdata[[j]]))$mtime)
    #   }
    #   latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
    #   load(file = paste0("04_shiny/x_results/",latest_file))
    # } else{
    #   stop("shiny: can't find underlying simulation to load!")
    # }
  }
  #NB: need new function to subset
  
  ### Subset data to this_configuration
  this_configuration <- default_configuration[! names(default_configuration) %in% c({{var_1}},{{var_2}})]
  to_plot <-  filter_scenarios(data,this_configuration)
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
    return(filter_scenarios(data,this_configuration,warning_search = 1))
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
  to_plot <- to_plot %>% ungroup()
  if ("pathogen" %in% names(to_plot)) to_plot <- to_plot %>% group_by(pathogen)
  to_plot <- to_plot %>%
    group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity, .add = TRUE) %>%
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
    select(-phase,-supply,-incidence) %>%
    rename(baseline = cumulative_incidence)
  
  join_by_list <- c("time", "setting","vaccine_delivery_start_date", "R0", "infection_derived_immunity", "rollout_modifier", "vaccine_derived_immunity")
  if("pathogen" %in% colnames(to_plot)) join_by_list = c("pathogen",join_by_list)
  
  to_plot <- to_plot %>%
    filter(phase != "no vaccine") %>%
    left_join(workshop, relationship = "many-to-many", by = join_by_list) %>%
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
    theme(legend.position="bottom") +
    xlab("days since detection")
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
  #free y-axis
  if (free_yaxis) left_plot <- left_plot + facet_grid(.data[[var_1]] ~. , scales = "free_y")
  
  
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
    
    var_1_type <- data.frame(to_plot)
    var_1_type <- typeof(var_1_type[,{{var_1}}])
    if (var_1_type == "character") to_plot$pathogen <- factor(to_plot$pathogen, levels = rev(unique(to_plot$pathogen)))
    
    right_plot <-  ggplot(to_plot) + 
      geom_tile(aes(x=.data[[var_1]],y=phase,fill=vaccine_effect)) +
      geom_text(aes(x=.data[[var_1]],y=phase,label = round(vaccine_effect,digits=2)), color = "black", size = 4)+ 
      scale_fill_gradientn(colours=c("white","orange","red","dark red"), limits=c(0,1)) +
      ylab("strategy") +
      coord_flip() +
      theme_bw() +
      theme(legend.position="bottom")
    if (var_1_type != "character") right_plot <- right_plot + scale_x_reverse()
    
    if (is.na(var_2) == FALSE){
      right_plot <- right_plot + 
        ylab(gsub("_"," ",var_2))
    }
    
    ggarrange(left_plot,right_plot,nrow = 1)
    
  } else{
    left_plot  
  }
  
}