
#### SETUP #####################################################################
#rm(list = ls())
require(tidyverse); require(ggpubr);require(shiny); require(shinyWidgets); require(reactlog); require(waiter); require(bslib); require(bsicons); require(scales)
options(scipen = 1000) #turn off scientific notation
for (function_script in list.files(path=paste0(gsub("/04_shiny","",getwd()),"/02_functions/"), full.name = TRUE)){source(function_script)}
load(file =  paste0(gsub("/04_shiny","",getwd()),"/01_inputs/age_specific_severity_MASTER.Rdata"))
is_local <- Sys.getenv("SHINY_PORT") == "" #boolean of whether this shiny is being hosted locally or on posit.co
################################################################################




##### CONFIGURE CHOICES ########################################################
CHOICES = list(
  incidence_statistic = 
    c("incidence" = "incidence",
      "cumulative incidence" = "cumulative_incidence",
      "cumulative incidence averted" = "cumulative_incidence_averted"), 
  outcome = c("infections","deaths","presentations"),
  vaccination_strategies = 
    c("uniform",
      "older adults followed by all adults","adults then children","children then adults",
      "step up", "step down"),
  R0 = seq(2,6) ,
  vaccine_delivery_start_date = c(50,100,150,200) ,
  supply = 
    c("20%" = 0.2,
      "50%" = 0.5,
      "80%" = 0.8), 
  immunity = 
    c("25%"  = 0.25,
      "50%"  = 0.5,
      "75%"  = 0.75,
      "100%" = 1.0), 
  rollout_modifier = 
    c("x0.5 COVID-19 capacity"  = 0.5,
      "at COVID-19  capacity"   = 1,
      "x2.0 COVID-19  capacity" = 2),
  outcome_threshold = c(1,2,5,10,25),
  gen_interval = c(7,14,21,28),
  IR_outcome = c("1%"  = 0.01, # COVID-19 WT and influenza like
                 "10%" = 0.1,  # diptheria and SARS like
                 "25%" = 0.25, # Lassa fever and MERS like
                 "50%" = 0.5,  # TB, cholera, JEV and HIV like
                 "65%" = 0.65  # plague and Ebola like
                 #NB: need to include est for presentations!
  ), 
  develop_outcome = c(7,14,21,28)
)
################################################################################




##### FUNCTION DEFINITIONS #####################################################
make_checkboxGroupButtons <- function(this_var,this_label,these_choices,this_var_selected){
  checkboxGroupButtons(inputId = this_var,
                       label = this_label,
                       choices = these_choices,
                       selected = this_var_selected)
}
make_prettySwitch <- function(this_variable, this_label, default = TRUE){
  prettySwitch(
    label = this_label,
    inputId = this_variable,
    value = default,
    status = "success",
    fill = TRUE
  )
}
make_prettyRadioButtons <- function(this_variable, this_label,these_choices,this_var_selected){
  prettyRadioButtons(
    inputId = this_variable,
    label = this_label, 
    choices = these_choices,
    selected = this_var_selected,
    inline = TRUE, 
    fill = TRUE
  )
}

pathogen_characteristics_inputs <- list(
  make_checkboxGroupButtons("R0", "Basic reproduction number:", CHOICES$R0, 2),
  uiOutput("TOGGLES_project_deaths_age_distribution"),
  make_prettyRadioButtons("IR_outcome", "Infection fatality ratio (%):", CHOICES$IR_outcome, 0.01),
  make_checkboxGroupButtons("infection_derived_immunity", "Infection-derived protection against re-infection:", CHOICES$immunity, 1),
  make_checkboxGroupButtons("vaccine_derived_immunity","Vaccine effectiveness against infection:", CHOICES$immunity, 1),
  uiOutput("TOGGLES_project_deaths_VE")
)

vaccination_strategy_inputs <- list(
  selectInput(inputId = "vaccination_strategies",label = "Vaccination strategies:", 
              choices = CHOICES$vaccination_strategies, selected = "uniform", multiple = TRUE),
  make_checkboxGroupButtons("vaccine_delivery_start_date", "Days between pathogen detected and vaccine first delivered:", 
                            CHOICES$vaccine_delivery_start_date, 100),
  make_checkboxGroupButtons("supply", "Vaccine supply (% population):", CHOICES$supply, 0.2),
  make_checkboxGroupButtons("rollout_modifier", "Rollout speed:", CHOICES$rollout_modifier, 1),
  make_prettySwitch("daily_vaccine_delivery_realistic","gradual increase (mirroring COVID-19)", default = FALSE)
)

pathogen_detection_inputs <- list(
  selectInput(inputId = "detection_outcome",
              label = "Outcome used for detection:",
              choices = c("deaths","presentations"),
              selected = "deaths"),
  make_prettyRadioButtons("outcome_threshold", "Threshold number of this outcome for detection:", CHOICES$outcome_threshold, 2),
  make_prettyRadioButtons("gen_interval", "Generation interval (days):", CHOICES$gen_interval, 7),
  make_prettyRadioButtons("develop_outcome", "Time to developing outcome (days):", CHOICES$develop_outcome, 14) 
)

plot_configuration_inputs <- list(
  selectInput(inputId = "this_outcome",
              label = "Outcome:",
              choices = CHOICES$outcome),
  selectInput(inputId = "yaxis_title",
              label = "Incidence statistic:",
              choices = CHOICES$incidence_statistic,
              selected = "incidence"),
  make_prettySwitch("free_yaxis", "free y-axis"),
  make_prettySwitch("display_impact_heatmap", "heatmap"),
  make_prettySwitch("display_severity_curve", "severity curve", default = FALSE),
  make_prettySwitch("display_age_proportion_on_severity_curve", "age proportions on severity curve", default = FALSE ),
  make_prettySwitch("display_vaccine_availability", "dashed line of vaccine availability" ),
  make_prettySwitch( "display_end_of_healthcare_worker_delivery", "dashed line denoting end of healthcare worker delivery"),
  #make_prettySwitch("colour_healthcare_workers_phase","colour healthcare worker delivery"),
  uiOutput("SWITCH_plot_dimensions")
)
################################################################################




##### USER INTERFACE DEFINITION ################################################
ui <- page_sidebar(
  
  title = "Mathematical modelling of future pandemics to assist with Indonesian pandemic preparedness",
  
  sidebar = sidebar(
    title = "User toggles",
    width = 500,
    accordion(
      accordion_panel(title = "Characteristics of the pathogen",
                      icon = bsicons::bs_icon("virus2"),
                      pathogen_characteristics_inputs
      ),
      
      accordion_panel(title = "Vaccination strategies",
                      icon = bsicons::bs_icon("universal-access"),
                      vaccination_strategy_inputs
      ),
      
      accordion_panel(title = "Detection of pathogen",
                      icon = bsicons::bs_icon("stopwatch"),
                      pathogen_detection_inputs
      ),
      
      accordion_panel(title = "Plot configuration",
                      icon = bsicons::bs_icon("toggles"),
                      plot_configuration_inputs
      ), 
      open = FALSE
    )
  ),

  ### MAINPANEL
  waiter::useWaiter(),
  
  verbatimTextOutput ("test"),
  textOutput("test2"),
  textOutput("WARNING_no_plot"),
  plotOutput("OUTPUT_plot", height = "800px"),
  actionButton(inputId = "update_plot",
               label = "Update plot")

)
################################################################################




#### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
 # output$test <- renderText ({
 #   indicator_plot_ready
 #   })
 output$test2 <- renderText ({
   # paste(sapply(c(input$R0, input$outcome_threshold,input$gen_interval,input$IR_outcome,input$develop_outcome),is.numeric))
   # paste(input$R0, input$outcome_threshold,input$gen_interval,input$IR_outcome,input$develop_outcome)
   # paste(sapply(sapply(c(input$R0, input$outcome_threshold,input$gen_interval,input$IR_outcome,input$develop_outcome),as.numeric),is.numeric))
   # estimate_days_to_detection (
   #   as.numeric(input$outcome_threshold), # threshold number of this outcome for detection
   #   as.numeric(input$gen_interval),      # generation interval (days)
   #   as.numeric(input$IR_outcome),        # incidence rate for this outcome
   #   as.numeric(input$develop_outcome),   # time to developing outcome (days)
   #   as.numeric(input$R0)                # basic reproduction number
   # )
 })
  
  
  ### Conditional UI components
  output$TOGGLES_project_deaths_age_distribution <- renderUI({
    if(input$this_outcome == "deaths") selectInput(inputId = "deaths_age_distribution",label = "Age distribution of severity:", 
                                                 choices = sort(unique(age_specific_severity_MASTER$pathogen)), selected = "Plague", multiple = TRUE)
  })
  output$TOGGLES_project_deaths_VE <- renderUI({
    if(input$this_outcome == "deaths") make_checkboxGroupButtons("deaths_VE","Vaccine effectiveness against death:", CHOICES$immunity, 1) 
  })
  # output$TOGGLES_project_comorb_increased_risk <- renderUI({
  #   if(input$this_outcome == "deaths")  numericInput(inputId = "comorb_increased_risk", label = "Increased RR of individuals with comorbidities:", value = 1)
  # })
  output$SWITCH_plot_dimensions <- renderUI({
    if(select_var(1)[[1]] != "R0" & length(count_plot_dimensions())>1) make_prettySwitch("switch_plot_dimensions","switch plot dimensions", default = FALSE)
  })
  
  #call_waiter: creates spinner while waiting for plot to load
  call_waiter <- function(this_output){
    waiter::Waiter$new(
      id = this_output,
      html = spin_3(), 
      color = transparent(.5)
    )$show()
  }
  #count_plot_dimensions: checks which input variables have multiple values selected
  count_plot_dimensions <- reactive({
    plot_dimension_vector = c()
    
    if (length(input$R0)>1)                          plot_dimension_vector = c(plot_dimension_vector,"R0")
    if (length(input$vaccine_delivery_start_date)>1) plot_dimension_vector = c(plot_dimension_vector,"vaccine_delivery_start_date")
    if (length(input$supply)>1)                      plot_dimension_vector = c(plot_dimension_vector,"supply")
    if (length(input$rollout_modifier)>1)            plot_dimension_vector = c(plot_dimension_vector,"rollout_modifier")
    if (length(input$infection_derived_immunity)>1)  plot_dimension_vector = c(plot_dimension_vector,"infection_derived_immunity")
    if (length(input$vaccine_derived_immunity)>1)    plot_dimension_vector = c(plot_dimension_vector,"vaccine_derived_immunity")
    if (input$this_outcome == "deaths" & length(input$deaths_age_distribution)>1)    plot_dimension_vector = c(plot_dimension_vector,"pathogen")
    
    if (is.null(input$switch_plot_dimensions) == FALSE && input$switch_plot_dimensions == TRUE) plot_dimension_vector <- rev(plot_dimension_vector)
    
    plot_dimension_vector
  }) 
  #select_var: tells you which variable has multiple values selected, and the range specified
  select_var <- function(num){
    
    plot_dimensions <- count_plot_dimensions()
    
    if(length(plot_dimensions)>(num-1)) this_var = plot_dimensions[num] else if (num ==1) this_var = "R0" else this_var = NA
    
    if (is.na(this_var) == FALSE){
      if (this_var == "R0") {
        this_var_range = input$R0
      } else if (this_var == "vaccine_delivery_start_date") {
        this_var_range = input$vaccine_delivery_start_date
      } else if (this_var == "supply") {
        this_var_range = input$supply
      } else if (this_var == "infection_derived_immunity") {
        this_var_range = input$infection_derived_immunity
      } else if (this_var == "rollout_modifier") {
        this_var_range = input$rollout_modifier
      } else if (this_var == "vaccine_derived_immunity") {
        this_var_range = input$vaccine_derived_immunity
      } else {
        this_var_range = NA
      }
    } else{
      this_var_range = NA
    }
    list(this_var,this_var_range)

  }
  #call_plot: calls the plotting function after checking all required inputs provided
  call_plot <- function(){
   if ((input$this_outcome %in% c("presentations","infections") |
        (
          is.character(input$deaths_age_distribution) &
          is.numeric(input$deaths_VE)
        )) &
       is.null(input$R0) == FALSE &
       is.null(input$outcome_threshold) == FALSE &
       is.null(input$gen_interval) == FALSE &
       is.null(input$IR_outcome) == FALSE &
       is.null(input$develop_outcome) == FALSE &
       is.null(input$vaccine_delivery_start_date) == FALSE &
       is.null(input$supply) == FALSE &
       is.null(input$infection_derived_immunity) == FALSE &
       is.null(input$rollout_modifier) == FALSE &
       is.null(input$vaccine_derived_immunity) == FALSE) {
     
     this_TOGGLES_project_deaths <- list(
       point_estimate =  as.numeric(input$IR_outcome),
       age_distribution = input$deaths_age_distribution,
       VE_death =  as.numeric(input$deaths_VE),
       comorb_increased_risk = 1
     )
     if (input$this_outcome %in% c("presentations","infections")) this_TOGGLES_project_deaths <- list()
     
     withProgress(message = "running underlying simulations",{
       plot_simulations(
         var_1 = select_var(1)[[1]],
         var_2 = select_var(2)[[1]],
         yaxis_title = input$yaxis_title,
         this_outcome = input$this_outcome,
         TOGGLES_project_deaths = this_TOGGLES_project_deaths, 
         var_1_range = select_var(1)[[2]],
         var_2_range = select_var(2)[[2]],
         default_configuration =
           list(
             R0 = as.numeric(input$R0),
             outcome_threshold = as.numeric(input$outcome_threshold),
             gen_interval = as.numeric(input$gen_interval), 
             IR_outcome = as.numeric(input$IR_outcome),  
             develop_outcome = as.numeric(input$develop_outcome), 
             vaccine_delivery_start_date = as.numeric(input$vaccine_delivery_start_date),
             phase = c(input$vaccination_strategies,"healthcare workers", "no vaccine"),
             supply = as.numeric(input$supply),
             infection_derived_immunity =  as.numeric(input$infection_derived_immunity),
             rollout_modifier =  as.numeric(input$rollout_modifier),
             vaccine_derived_immunity =  as.numeric(input$vaccine_derived_immunity),
             daily_vaccine_delivery_realistic = input$daily_vaccine_delivery_realistic
           ),
         
         free_yaxis = input$free_yaxis,
         display_impact_heatmap = input$display_impact_heatmap,
         display_severity_curve = input$display_severity_curve,
         display_age_proportion_on_severity_curve = input$display_age_proportion_on_severity_curve,
         display_var_1 = 0,
         colour_healthcare_workers_phase = 0, #input$colour_healthcare_workers_phase,
         display_vaccine_availability = input$display_vaccine_availability,
         display_end_of_healthcare_worker_delivery = input$display_end_of_healthcare_worker_delivery,
         simulations_source = "generate"
       )
     })
   }
  }
  
  #output_plot when update_button clicked OR when app initialised and last UI button (vaccine_derived_immunity) created
  output_plot <- eventReactive({input$update_plot|is.null(input$vaccine_derived_immunity) == FALSE},{
    call_waiter("OUTPUT_plot")
    call_plot()
  })
  
  output$WARNING_no_plot <- renderText({

    plot_dimensions <- count_plot_dimensions()

    if(length(plot_dimensions)>2) validate(paste0("You have selected multiple values for more than two variables (",length(plot_dimensions), " variables with multiple values selected) .",
                                                  "\nTo create a plot please deselect values values for ",length(plot_dimensions)-2," variable(s)."))

    check_plot_exists <- output_plot
    if(is.character(check_plot_exists)) {
      validate(paste("\nNote: The underlying simulation for this plot does not exist. There are no simulations available for the selected value of:",check_plot_exists ))
    }
  })
  
  output$OUTPUT_plot <-   renderPlot({
    plot_dimensions <- count_plot_dimensions()
    if (length(plot_dimensions)<3)  print(output_plot())
  },
  res = 96)
  
}


shinyApp(ui, server) #run application!