
#### SETUP #####################################################################
#rm(list = ls())
require(tidyverse); require(ggpubr);require(shiny); require(shinyWidgets); require(reactlog); require(waiter)
options(scipen = 1000) #turn off scientific notation
for (function_script in list.files(path=paste0(gsub("/04_shiny","",getwd()),"/02_functions/"), full.name = TRUE)){source(function_script)}

path_stem <- paste0(gsub("/04_shiny","",getwd()),"/04_shiny/x_results/")
list_poss_Rdata = list.files(
  path = path_stem,
  pattern = "ship_log*"
)
if (length(list_poss_Rdata) > 0) {
  list_poss_Rdata_details = double()
  for (j in 1:length(list_poss_Rdata)) {
    list_poss_Rdata_details = rbind(list_poss_Rdata_details,
                                    file.info(paste0(path_stem, list_poss_Rdata[[j]]))$mtime)
  }
  latest_file = list_poss_Rdata[[which.max(list_poss_Rdata_details)]]
  load(file = paste0(path_stem,latest_file))
  load(file = paste0(path_stem,gsub("ship_log","ship_log_key",latest_file))) #load accompanying key
} else{
  stop(paste0("(R Shiny) app: can't find underlying simulation to load! Searching:", path_stem))
}
load(file =  paste0(gsub("/04_shiny","",getwd()),"/01_inputs/age_specific_severity_MASTER.Rdata"))
################################################################################




##### CONFIGURE CHOICES ########################################################
CHOICES = list(
  variable = 
    c("basic reproduction number" = "R0",
      "vaccine delivery start date" = "vaccine_delivery_start_date",
      "infection derived immunity" = "infection_derived_immunity",
      "rollout modifier" = "rollout_modifier",
      "vaccine derived immunity" = "vaccine_derived_immunity"), 
  incidence_statistic = 
    c("incidence" = "incidence",
      "cumulative incidence" = "cumulative_incidence",
      "cumulative incidence averted" = "cumulative_incidence_averted"), 
  vaccination_strategies = unique(ship_log$phase[! ship_log$phase %in% c("no vaccine", "healthcare workers")]),
  R0 = unique(ship_log_key$R0) ,
  vaccine_delivery_start_date = unique(ship_log_key$vaccine_delivery_start_date) ,
  supply = 
    c("20%" = 0.2,
      "50%" = 0.5,
      "80%" = 0.8), 
  infection_derived_immunity = 
    c("75%" = 0.75,
      "100%" = 1.0), 
  rollout_modifier = 
    c("x0.5 capacity" = 0.5,
      "at capacity" = 1,
      "x2.0 capacity" = 2),
  vaccine_derived_immunity =
    c("75%" = 0.75,
      "100%" = 1.0)
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
################################################################################




##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  titlePanel("Mathematical modelling of future pandemics to assist with Indonesian pandemic preparedness "),
  
  sidebarLayout(
    
    ### Widgets ################################################################ 
    sidebarPanel( width = 3,
                  
                  selectInput(inputId = "yaxis_title",
                              label = "Incidence statistic:",
                              choices = CHOICES$incidence_statistic,
                              selected = "incidence"),
                  selectInput(inputId = "this_outcome",
                              label = "Outcome:",
                              choices = c("cases","deaths","presentations")),
                  selectInput(inputId = "vaccination_strategies",label = "Vaccination strategies:", 
                              choices = CHOICES$vaccination_strategies, selected = "uniform", multiple = TRUE),
                  
                  #TOGGLES_project_severe_disease
                  uiOutput("TOGGLES_project_severe_disease_point_estimate"),
                  uiOutput("TOGGLES_project_severe_disease_age_distribution"),
                  uiOutput("TOGGLES_project_severe_disease_VE"),
                  uiOutput("TOGGLES_project_comorb_increased_risk"),
                  
                  make_checkboxGroupButtons("R0", "Basic reproduction number:", CHOICES$R0, 2),
                  make_checkboxGroupButtons("vaccine_delivery_start_date", "Days between pathogen detected and vaccine first delivered:", 
                                            CHOICES$vaccine_delivery_start_date, 100),
                  make_checkboxGroupButtons("supply", "Vaccine supply (% population):", CHOICES$supply, 0.2),
                  make_checkboxGroupButtons("rollout_modifier", "Rollout speed:", CHOICES$rollout_modifier, 1),
                  make_prettySwitch("daily_vaccine_delivery_realistic","gradual increase (mirroring COVID-19)", default = FALSE),
                  make_checkboxGroupButtons("infection_derived_immunity", "Protection from infection-derived immunity:", CHOICES$infection_derived_immunity, 1),
                  make_checkboxGroupButtons("vaccine_derived_immunity","Protection from vaccine-derived immunity:", CHOICES$vaccine_derived_immunity, 1),

                  h5(strong("Display:")),
                  make_prettySwitch("free_yaxis","free y-axis"),
                  make_prettySwitch("display_impact_heatmap","heatmap"),
                  make_prettySwitch("display_severity_curve","severity curve", default = FALSE),
                  make_prettySwitch("display_age_proportion_on_severity_curve","age proportions on severity curve", default = FALSE),
                  make_prettySwitch("display_vaccine_availability","date of vaccine availability"),
                  make_prettySwitch("display_end_of_healthcare_worker_delivery","end of healthcare worker delivery"),
                  make_prettySwitch("colour_healthcare_workers_phase","colour healthcare worker delivery"),
                  make_prettySwitch("switch_plot_dimensions","switch plot dimensions", default = FALSE),
                  
    ),
    
    
    
    ### Outputs ################################################################
    mainPanel( width = 9,
               
               waiter::useWaiter(),
               
               verbatimTextOutput ("test"),
               textOutput("test2"),
               
               textOutput("WARNING_no_plot"),
               plotOutput("OUTPUT_plot", height = "800px"),
               actionButton(inputId = "update_plot",
                            label = "Update plot")
               
    )
  )
)
################################################################################




#### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
 # output$test <- renderText ({
 #   indicator_plot_ready
 #   })
 # output$test2 <- renderText ({
 #   count_plot_dimensions()
 # })
  
  
  ### Conditional UI components
  output$TOGGLES_project_severe_disease_point_estimate <- renderUI({
    if(input$this_outcome != "cases") numericInput(inputId = "severe_disease_point_estimate", label = "Population-level estimate (%):", value = 0.01)
  })
  output$TOGGLES_project_severe_disease_age_distribution <- renderUI({
    if(input$this_outcome != "cases") selectInput(inputId = "severe_disease_age_distribution",label = "Age distribution:", 
                                                 choices = sort(unique(age_specific_severity_MASTER$pathogen)), selected = "Plague", multiple = TRUE)
  })
  output$TOGGLES_project_severe_disease_VE <- renderUI({
    if(input$this_outcome != "cases") numericInput(inputId = "severe_disease_VE", label = "Vaccine effectiveness against severe disease:", value = 1)
  })
  # output$TOGGLES_project_comorb_increased_risk <- renderUI({
  #   if(input$this_outcome != "cases")  numericInput(inputId = "comorb_increased_risk", label = "Increased RR of individuals with comorbidities:", value = 1)
  # })
  
  
  #count_plot_dimensions: checks which input variables have multiple values selected
  count_plot_dimensions <- reactive({
    plot_dimension_vector = c()
    
    if (length(input$R0)>1)                          plot_dimension_vector = c(plot_dimension_vector,"R0")
    if (length(input$vaccine_delivery_start_date)>1) plot_dimension_vector = c(plot_dimension_vector,"vaccine_delivery_start_date")
    if (length(input$supply)>1)                      plot_dimension_vector = c(plot_dimension_vector,"supply")
    if (length(input$rollout_modifier)>1)            plot_dimension_vector = c(plot_dimension_vector,"rollout_modifier")
    if (length(input$infection_derived_immunity)>1)  plot_dimension_vector = c(plot_dimension_vector,"infection_derived_immunity")
    if (length(input$vaccine_derived_immunity)>1)    plot_dimension_vector = c(plot_dimension_vector,"vaccine_derived_immunity")
    if (input$this_outcome != "cases" & length(input$severe_disease_age_distribution)>1)    plot_dimension_vector = c(plot_dimension_vector,"pathogen")
    
    if (input$switch_plot_dimensions == TRUE) plot_dimension_vector <- rev(plot_dimension_vector)
    
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
   if ((input$this_outcome == "cases" |
        (
          is.numeric(input$severe_disease_point_estimate) &
          is.character(input$severe_disease_age_distribution) &
          is.numeric(input$severe_disease_VE)
        )) &
       is.null(input$R0) == FALSE &
       is.null(input$vaccine_delivery_start_date) == FALSE &
       is.null(input$supply) == FALSE &
       is.null(input$infection_derived_immunity) == FALSE &
       is.null(input$rollout_modifier) == FALSE &
       is.null(input$vaccine_derived_immunity) == FALSE) {
     
     this_TOGGLES_project_severe_disease <- list(
       point_estimate =  input$severe_disease_point_estimate,
       age_distribution = input$severe_disease_age_distribution,
       VE_severe_disease =  input$severe_disease_VE,
       comorb_increased_risk = 1
     )
     if (input$this_outcome == "cases") this_TOGGLES_project_severe_disease <- list()
     
     plot_simulations(
       var_1 = select_var(1)[[1]],
       var_2 = select_var(2)[[1]],
       yaxis_title = input$yaxis_title,
       this_outcome = input$this_outcome,
       TOGGLES_project_severe_disease = this_TOGGLES_project_severe_disease, 
       var_1_range = select_var(1)[[2]],
       var_2_range = select_var(2)[[2]],
       default_configuration =
         list(
           R0 = input$R0,
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
       colour_healthcare_workers_phase = input$colour_healthcare_workers_phase,
       display_vaccine_availability = input$display_vaccine_availability,
       display_end_of_healthcare_worker_delivery = input$display_end_of_healthcare_worker_delivery,
       load_simulations = FALSE
     )
   }
  }
  
  #output_plot when update_button clicked OR when app initialised and last UI button (vaccine_derived_immunity) created
  output_plot <- eventReactive({input$update_plot|is.null(input$vaccine_derived_immunity) == FALSE},{call_plot()})
  
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