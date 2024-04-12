
require(scales)

plot_simulations <- function(
    
    # configure underlying simulations
    simulations_source = "load", #options: "load", "memory", "generate"
    default_configuration =
      list(
        # time to detection variables
        setting = "Indonesia",
        R0 = 2,            # basic reproduction number
        outcome_threshold = 2, # threshold number of this outcome for detection
        gen_interval = 7,      # generation interval (days)
        IR_outcome = 0.01,     # incidence rate for this outcome
        develop_outcome = 14,  # time to developing outcome (days)
        # vaccine program variables
        vaccine_delivery_start_date = 100,
        phase = c(
          "older adults followed by all adults",
          "adults then children",               
          "children then adults",                
          "step up",                            
          "step down",                           
          "uniform", 
          "healthcare workers",
          "no vaccine"
        ),
        supply = c(0.2),
        rollout_modifier = 2,
        daily_vaccine_delivery_realistic = FALSE,
        vaccine_derived_immunity = 1,
        # all other variables
        infection_derived_immunity = 1
      ),    
    
    # configure plot
    var_1, 
    var_2 = NA,
    yaxis_title, #options: incidence, cumulative_incidence, cumulative_incidence_averted
    this_outcome = "cases", #options: "cases","deaths"
    TOGGLES_project_severe_disease = list(),
    var_1_range = NA,
    var_2_range = NA,

    # modify plot aesthetics
    free_yaxis = FALSE,
    display_impact_heatmap = 1, #options: 0 (no), 1 (yes)
    display_severity_curve = 0,
    display_age_proportion_on_severity_curve = 0,
    display_var_1 = 1,
    colour_healthcare_workers_phase = 1, #options: 0 (no), 1 (yes)
    display_vaccine_availability = 1, #options: 0 (no), 1 (yes)
    display_end_of_healthcare_worker_delivery = 1  #options: 0 (no), 1 (yes)
    
){
  
  ### FORCE
  if (exists("var_2") == FALSE | is.null(var_2)) var_2 = NA
  if (is.na(var_2) == FALSE &&  var_2 == "none") var_2 = NA
  if (is.na(var_2)) var_2_range = NA
  if (this_outcome == "cases") TOGGLES_project_severe_disease = list()
  if (this_outcome == "cases") display_severity_curve = 0
  if (this_outcome != "cases" & length(TOGGLES_project_severe_disease) == 0) stop("plot_simulations: you have selected to plot severe outcomes but not specified TOGGLES_project_severe_disease")
  
  
  ### Load simulation
  this_configuration = default_configuration
  
  # remove var_1 and var_2 this_configuration
  this_configuration = this_configuration[! names(this_configuration) %in% c({{var_1}},{{var_2}})]
  
  # use var_1_range and var_2_range in this_configuration
  if (length(var_1_range)>0){
    if (is.na(var_1_range[1]) == FALSE) this_configuration = c(this_configuration, var_1 = list(var_1_range)); names(this_configuration)[names(this_configuration) == "var_1"] = var_1
  }
  if (length(var_2_range)>0){
    if (is.na(var_2_range[1]) == FALSE) this_configuration = c(this_configuration, var_2 = list(var_2_range)); names(this_configuration)[names(this_configuration) == "var_2"] = var_2
  }
  if (simulations_source == "generate" & is.na(var_1_range[1]) & var_1 != "pathogen") stop("you must specify var_1_range to generate this simulation")
  if (simulations_source == "generate" & is.na(var_2) == FALSE & length(var_2_range) == 0) stop("you must specify var_2_range to generate this simulation")
  
  if (! "setting" %in% this_configuration) this_configuration$setting = "Indonesia"
  if (! "daily_vaccine_delivery_realistic" %in% this_configuration) this_configuration$daily_vaccine_delivery_realistic = FALSE
  
  if (simulations_source != "generate"){
    ROUND_days_to_detection = 1
    
    # calculate days to detection
    workshop = crossing(
      outcome_threshold = as.numeric(this_configuration$outcome_threshold),
      gen_interval = as.numeric(this_configuration$gen_interval),
      IR_outcome = as.numeric(this_configuration$IR_outcome),
      develop_outcome = as.numeric(this_configuration$develop_outcome),
      R0 = as.numeric(this_configuration$R0)
    ) %>%
      mutate(days_to_detection = estimate_days_to_detection(outcome_threshold,gen_interval,IR_outcome,develop_outcome,R0),
             days_to_detection = round(days_to_detection/ROUND_days_to_detection)*ROUND_days_to_detection) %>%
      select(days_to_detection,R0)
    this_configuration$days_to_detection <- workshop
    this_configuration <- this_configuration[! names(this_configuration) %in% c("outcome_threshold","gen_interval","IR_outcome","develop_outcome")]
  }

  include_strategies <- default_configuration$phase[! default_configuration$phase %in% c("healthcare workers","no vaccine" )]
  
  to_plot <- to_plot_loaded <- access_simulations(
    simulations_source,
    this_configuration,
    TOGGLES_project_severe_disease,
    outcome = this_outcome
    )
  
  
  ### Rename phase to include var_2 in plot if relevant
  if (is.na(var_2) == FALSE){
    if (length(include_strategies)>1){
      to_plot <- to_plot %>%
        mutate(phase = case_when(
          phase %in% c("no vaccine","healthcare workers") ~ phase,
          TRUE ~ paste(phase,"(",var_2,.data[[var_2]],")")
        ))
    } else{
      
      if (var_2 != "vaccine_delivery_start_date"){ # general case
        to_plot <- to_plot %>%
          mutate(phase = case_when(
            phase %in% c("no vaccine","healthcare workers") ~ phase,
            TRUE ~ paste(var_2,.data[[var_2]])
          ))
      } else if (var_2 == "vaccine_delivery_start_date"){
        to_plot <- to_plot %>%
          mutate(phase = case_when(
            phase %in% c("no vaccine","healthcare workers") ~ phase,
            TRUE ~ paste("vaccine delivery starting at",.data[[var_2]],"days")
          ))
      }
      
      to_plot$phase <- factor(to_plot$phase, levels = c(unique(to_plot$phase[to_plot$phase != "no vaccine"]) ,
                                                        "no vaccine" ))
    }
  } else{
    to_plot$phase <- factor(to_plot$phase, levels = c("older adults followed by all adults",
                                                      "children then adults" ,
                                                      "adults then children"  ,
                                                      "step down" ,
                                                      "step up" ,
                                                      "uniform",
                                                      "healthcare workers" ,
                                                      "no vaccine" ))
  }
  
  
  ### Send error if no simulations selected
  if (nrow(to_plot[to_plot$phase != "no vaccine",]) == 0){
    return(filter_scenarios(to_plot_loaded,this_configuration,warning_search = 1))
  }
  rm(to_plot_loaded)
  
  
  ### Collect information on the last date of healthcare worker rollout in case the explicit labelling of this phase is dropped later
  vline_data2 <- to_plot %>%
    filter(phase == "healthcare workers") %>%
    group_by(.data[[var_1]]) %>%
    summarise(end_of_healthcare_workers_phase = max(time), .groups = "keep") 
  
  
  ### Making plot option 1/3: Incidence vs time
  if (yaxis_title == "incidence"){
    
    if (colour_healthcare_workers_phase == 1){
      to_plot_left_plot <- to_plot
    } else if (colour_healthcare_workers_phase == 0){
      to_plot_left_plot <- to_plot %>%
        filter(! phase %in% c("healthcare workers"))
    }
    
    left_plot <- ggplot(to_plot_left_plot) +
      geom_line(aes(x=time,y=incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "") +
      guides(color = guide_legend(nrow = 2)) +
      facet_grid(.data[[var_1]] ~.) + 
      ylab(paste0("incidence (",this_outcome,")"))
  } 
  
  
  ### Calculate cumulative_incidence
  # manipulate data to cumulative data set - NB: can move to run_disease_model to save time for Shiny
  to_plot <- to_plot %>% ungroup()
  if ("pathogen" %in% names(to_plot)) to_plot <- to_plot %>% group_by(pathogen)
  to_plot <- to_plot %>%
    group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity, .add = TRUE) %>%
    arrange(time) %>%
    mutate(cumulative_incidence = cumsum(incidence))
  
  
  ### Isolate healthcare worker period if colour_healthcare_workers_phase == 1 
  if (colour_healthcare_workers_phase == 1){
    #remove strategy before healthcare workers
    max_healthcare_workers_time = to_plot %>%
      filter(phase == "healthcare workers") %>%
      summarise(max_time = max(time), .groups = "keep")%>%
      ungroup() %>%
      select(-phase,-supply)
    cumulative_no_vax = to_plot %>%
      filter(time == vaccine_delivery_start_date - 1 & 
               phase == "no vaccine") %>%
      reframe(cum_no_vax = cumulative_incidence) %>%
      ungroup() %>%
      select(-phase,-supply)
    
    join_by_vars = c("setting", "vaccine_delivery_start_date","R0", "infection_derived_immunity", "rollout_modifier", "vaccine_derived_immunity")
    if ("pathogen" %in% names(to_plot)) join_by_vars = c(join_by_vars,"pathogen")
    
    to_plot <- to_plot %>% 
      left_join(max_healthcare_workers_time, by = join_by_vars) %>%
      left_join(cumulative_no_vax, by = join_by_vars) %>%
      filter(!(time <= max_time & ! phase %in% c("no vaccine","healthcare workers"))) %>%
      mutate(cumulative_incidence = case_when(
        phase == "healthcare workers" ~ cumulative_incidence + cum_no_vax,
        TRUE ~ cumulative_incidence
      )) %>%
      select(-max_time, - cum_no_vax)
    
  } else if (colour_healthcare_workers_phase == 0){
    to_plot <- to_plot %>%
      filter(! phase %in% c("healthcare workers"))
  }
  
  
  ### Making plot option 2/3: Cumulative incidence vs time
  if (yaxis_title == "cumulative_incidence"){
    to_plot_left_plot <- to_plot
    left_plot <- ggplot(to_plot_left_plot) + 
      geom_line(aes(x=time,y=cumulative_incidence,color=as.factor(phase)),linewidth = 1.25)  +
      labs(color="", linetype = "")+ 
      ylab(paste0("cumulative incidence (",this_outcome,")")) +
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
      ylab(paste0("cumulative incidence averted (",this_outcome,")")) +
      xlim(0,max(to_plot$time)) + 
      guides(color = guide_legend(nrow = 2)) +
      facet_grid(.data[[var_1]] ~.)
  }
  
  
  ### Apply plotting visuals
  #uniform colours, NB: caution as these names are user defined in the command_deck (will be fixed in the Shiny)
  defined_colour_palette <-  c("no vaccine" = "#F8766D", 
                               "healthcare workers"  = "#00BF7D" ,                  
                               "older adults followed by all adults" = "#3b94b2",
                               "adults then children" = "#76c3c4" ,              
                               "children then adults" ="#ebd829" ,               
                               "step up" = "#e1a500",                             
                               "step down" = "#00B0F6" ,                          
                               "uniform"  = "#00BA38")
  if(is.na(var_2) == FALSE){
    defined_colour_palette <- defined_colour_palette[1:length(unique(to_plot_left_plot$phase))]
    names(defined_colour_palette) <- unique(to_plot_left_plot$phase)
    if ("no vaccine" %in%  names(defined_colour_palette))  names(defined_colour_palette) <- c("no vaccine", names(defined_colour_palette)[ names(defined_colour_palette) != "no vaccine"])
    
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
  #dashed line for last day of healthcare worker delivery
  if (display_end_of_healthcare_worker_delivery == 1){
    left_plot <- left_plot + geom_vline(data = vline_data2, aes(xintercept = end_of_healthcare_workers_phase), linetype = "dashed")
  }
  #free y-axis
  if (free_yaxis) left_plot <- left_plot + facet_grid(.data[[var_1]] ~. , scales = "free_y")
  
  
  ### Make heatmap & output plot!
  if (display_impact_heatmap == 1){
    
    to_plot <- to_plot %>%
      filter(phase != "no vaccine" &
               time == max(to_plot$time)) %>%
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
      scale_fill_gradientn(colours=c("white","springgreen3","forestgreen"), limits=c(0,1)) +
      ylab("strategy") +
      coord_flip() +
      theme_bw() +
      theme(legend.position="bottom") +
      scale_y_discrete(labels = label_wrap(20))
       
    if (var_1_type != "character") right_plot <- right_plot + scale_x_reverse(labels = label_wrap(20)) else right_plot <- right_plot + scale_x_discrete(labels = label_wrap(20))
    
    if (is.na(var_2) == FALSE){
      right_plot <- right_plot + 
        ylab(gsub("_"," ",var_2))
    }
    
    result <- ggarrange(left_plot,right_plot,nrow = 1)
    
  } else{
    result <- left_plot  
  }
  
  
  ### Include severity_plot
  if (display_severity_curve == 1){
    
    extra_plot <- project_severe_disease(
      TOGGLES_project_severe_disease = TOGGLES_project_severe_disease, 
      return_severity = TRUE
    ) %>%
      filter(vaccination_status == FALSE & comorbidity == FALSE) %>%
      ggplot() +
      geom_col(aes(x=age_group,y=case_fatality_rate)) +
      facet_wrap(~ pathogen, ncol = 1,scales = "free") 
    
    if (display_age_proportion_on_severity_curve == 1){
      extra_plot <- extra_plot +
        geom_text(aes(x=age_group,y=case_fatality_rate ,label = as.character(round(pop_proportion ,digits=2))), 
                  color = "white", size = 4, position = position_stack(vjust = 0.5))
    }
    
    if (display_impact_heatmap == 1)  result <- ggarrange(left_plot,right_plot,extra_plot,nrow = 1) 
    if (display_impact_heatmap == 0)  result <- ggarrange(left_plot,extra_plot,nrow = 1) 
  }
  
  
  print(result)
}
