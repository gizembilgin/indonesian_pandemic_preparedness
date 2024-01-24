
require(tidyverse); require(ggpubr);require(shiny); require(shinyWidgets); require(reactlog); require(waiter)
options(scipen = 1000) #turn off scientific notation




##### CONFIGURE CHOICES ########################################################
CHOICES = list(
  variable = c("basic reproduction number" = "R0",
               "vaccine delivery start date" = "vaccine_delivery_start_date",
               "infection derived immunity" = "infection_derived_immunity",
               "rollout modifier" = "rollout_modifier",
               "vaccine derived immunity" = "vaccine_derived_immunity"),
  incidence_statistic = c("incidence" = "incidence",
                          "cumulative incidence" = "cumulative_incidence",
                          "cumulative incidence averted" = "cumulative_incidence_averted"),
  R0 = c(1,2,4,6,8) ,
  vaccine_delivery_start_date = c(50,100),
  supply = c(0.2,0.5,0.8), #COMEBACK need to include 0 automatically later
  infection_derived_immunity = c(0.75,1),
  rollout_modifier = c(0.5,1,2),
  vaccine_derived_immunity = c(0.75,1)
)
################################################################################




##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  titlePanel("Interactive mathematical modelling of future pandemics to assist with Indonesian pandemic preparedness "),
  
  sidebarLayout(
    
    ### Widgets ################################################################ 
    sidebarPanel( width = 3,
                  
                  selectInput(inputId = "INPUT_variable",
                                    label = "Which variable to vary:",
                                    choices = CHOICES$variable,
                                    selected = "R0"),
                  selectInput(inputId = "INPUT_yaxis_title",
                              label = "Which incidence statistic:",
                              choices = CHOICES$incidence_statistic,
                              selected = "incidence"),
                  
                  radioGroupButtons(inputId = "INPUT_R0",
                                    label = "Basic reproduction number:",
                                    choices = CHOICES$R0,
                                    selected = 2),
                  radioGroupButtons(inputId = "INPUT_vaccine_delivery_start_date",
                                    label = "Days between pathogen detected and vaccine first delivered:",
                                    choices = CHOICES$vaccine_delivery_start_date,
                                    selected = 100), 
                  radioGroupButtons(inputId = "INPUT_supply",
                                    label = "Vaccine supply (% population):",
                                    choices = CHOICES$supply,
                                    selected = 0.2), #COMEBACK have this display as a percentage
                  radioGroupButtons(inputId = "INPUT_rollout_modifier",
                                    label = "Roll out modifier:", #COMEBACK need to explain
                                    choices = CHOICES$rollout_modifier,
                                    selected = 1), 
                  radioGroupButtons(inputId = "INPUT_infection_derived_immunity",
                                    label = "Infection-derived immunity (%):",
                                    choices = CHOICES$infection_derived_immunity,
                                    selected = 1),   #COMEBACK have this display as a percentage
                  radioGroupButtons(inputId = "INPUT_vaccine_derived_immunity",
                                    label = "Strength of vaccine derived immunity (%):",
                                    choices = CHOICES$vaccine_derived_immunity,
                                    selected = 1),   #COMEBACK have this display as a percentage
                  
                  switchInput(
                    label = "display impact heatmap?",
                    inputId = "INPUT_display_impact_heatmap",
                    value = TRUE
                  ),
                  switchInput(
                    label = "display essential worker delivery?", #COMEBACK better names for these switches
                    inputId = "INPUT_display_essential_workers_phase",
                    value = TRUE
                  ),
                  switchInput(
                    label = "display dashed line of vaccine availability?",
                    inputId = "INPUT_display_vaccine_availability",
                    value = TRUE
                  ),
                  switchInput(
                    label = "display dashed line of end of essential worker delivery?",
                    inputId = "INPUT_display_end_of_essential_worker_delivery",
                    value = TRUE
                  ),
                  
                  
    ),
    
    
    
    ### Outputs ################################################################
    mainPanel( width = 9,
               
               waiter::useWaiter(),
               
               textOutput("test"),
               tableOutput("test2"),
               
               textOutput("WARNING_no_plot"),
               plotOutput("OUTPUT_plot", height = "800px")
               #COMEBACK provide error message if no simulation available
    )
  )
)
################################################################################




#### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
  # output$test <- renderText({
  #   #"test"
  #   #multiscenario_facet_plot(ship_log_completed,"R0","incidence",display_vaccine_availability = 1, display_end_of_essential_worker_delivery = 1)
  #   print(c(0,as.numeric(input$INPUT_supply)))
  #   })
  # 
  # output$test2 <- renderTable({
  #   #"test"
  #   multiscenario_facet_plot(ship_log_completed,"R0","incidence",display_vaccine_availability = 1, display_end_of_essential_worker_delivery = 1)
  # })
  

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
                                       display_essential_workers_phase = 1, #options: 0 (no), 1 (yes)
                                       display_vaccine_availability = 1, #options: 0 (no), 1 (yes)
                                       display_end_of_essential_worker_delivery = 1  #options: 0 (no), 1 (yes)
  ){
    
    # set default values of model configuration parameters
    default_configuration <- list(
        R0 = as.numeric(input$INPUT_R0),
        vaccine_delivery_start_date = as.numeric(input$INPUT_vaccine_delivery_start_date),
        supply = c(0,as.numeric(input$INPUT_supply)), #include 0 for no vaccine scenario
        infection_derived_immunity = as.numeric(input$INPUT_infection_derived_immunity),
        rollout_modifier = as.numeric(input$INPUT_rollout_modifier),
        vaccine_derived_immunity = as.numeric(input$INPUT_vaccine_derived_immunity)
      )

    # subset data to this_configuration
    this_configuration <- default_configuration[! names(default_configuration) %in% {{this_var}}]
    to_plot <-  configuration_filter(data,this_configuration)
    to_plot$phase <- factor(to_plot$phase, levels = c("older adults followed by all adults",
                                                      "children before adults",
                                                      "all adults at the same time",
                                                      "essential workers" ,
                                                      "no vaccine" ))
    
    if (nrow(to_plot[to_plot$phase != "no vaccine",]) == 0){
      return()
    }
    
    vline_data2 <- to_plot %>%
      filter(phase == "essential workers") %>%
      group_by(.data[[this_var]]) %>%
      summarise(end_of_essential_workers_phase = max(time)) 
    
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
    #NB: caution as these names are user defined in the command_deck (will be fixed in the Shiny)
    left_plot <- left_plot  +
      scale_colour_manual(values = 
                            c("no vaccine" = "#F8766D", 
                              "older adults followed by all adults" =  "#A3A500",
                              "essential workers" = "#00BF7D" ,
                              "all adults at the same time" = "#00B0F6",
                              "children before adults"  = "#E76BF3"))
    #dashed line for first day of vaccine availability 
    if (display_vaccine_availability == 1){
      vline_data <- to_plot %>%
        group_by({{this_var}}) %>%
        summarise(vaccine_delivery_start_date = unique(vaccine_delivery_start_date))
      left_plot <- left_plot + geom_vline(data = vline_data, aes(xintercept = vaccine_delivery_start_date), linetype = "dashed")
    }
    #dashed line for last day of essential worker delivery
    if (display_end_of_essential_worker_delivery == 1){
      left_plot <- left_plot + geom_vline(data = vline_data2, aes(xintercept = end_of_essential_workers_phase), linetype = "dashed")
    }
    
    
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
  
  #text to display when hospitalisation for molnupiravir
  output$WARNING_no_plot <- renderText({
    if(is.null(
      multiscenario_facet_plot(
        data = ship_log_completed,
        this_var = input$INPUT_variable,
        yaxis_title = input$INPUT_yaxis_title,
        display_impact_heatmap = input$INPUT_display_impact_heatmap,
        display_essential_workers_phase = input$INPUT_display_essential_workers_phase,
        display_vaccine_availability = input$INPUT_display_vaccine_availability,
        display_end_of_essential_worker_delivery = input$INPUT_display_end_of_essential_worker_delivery
      ))) {
      validate("\nNote: The underlying simulation for this plot does not exist ")
    }
  })
  
  #output plot
  output$OUTPUT_plot <- renderPlot({
    multiscenario_facet_plot(
      data = ship_log_completed,
      this_var = input$INPUT_variable, 
      yaxis_title = input$INPUT_yaxis_title,
      display_impact_heatmap = input$INPUT_display_impact_heatmap,
      display_essential_workers_phase = input$INPUT_display_essential_workers_phase, 
      display_vaccine_availability = input$INPUT_display_vaccine_availability,
      display_end_of_essential_worker_delivery = input$INPUT_display_end_of_essential_worker_delivery
      )
    },
    res = 96)
  
}


shinyApp(ui, server) #run application!