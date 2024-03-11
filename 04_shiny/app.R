
#### SETUP #####################################################################
#rm(list = ls())
require(tidyverse); require(ggpubr);require(shiny); require(shinyWidgets); require(reactlog); require(waiter)
options(scipen = 1000) #turn off scientific notation
for (function_script in list.files(path="02_functions/", full.name = TRUE)){source(function_script)}

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
} else{
  stop(paste0("(R Shiny) app: can't find underlying simulation to load! Searching:", path_stem))
}
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
  R0 = unique(ship_log$R0) ,
  vaccine_delivery_start_date = unique(ship_log$vaccine_delivery_start_date) ,
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




##### USER INTERFACE DEFINITION ################################################
ui <- fluidPage(
  
  titlePanel("Mathematical modelling of future pandemics to assist with Indonesian pandemic preparedness "),
  
  sidebarLayout(
    
    ### Widgets ################################################################ 
    sidebarPanel( width = 3,
                  radioGroupButtons(inputId = "this_output",
                                    label = "Outcome:",
                                    choices = c("cases","deaths","presentations"),
                                    justified = TRUE),
                  uiOutput("severe_disease_pt_est_input"),
                  uiOutput("severe_disease_age_dn_input"),
                  uiOutput("severe_disease_comorb_increased_risk"),
                  selectInput(inputId = "var_1",
                                    label = "Variable to vary:",
                                    choices = CHOICES$variable,
                                    selected = "R0"),
                  selectInput(inputId = "var_2",
                              label = "Second variable to vary (optional):",
                              choices = CHOICES$variable,
                              selected = FALSE),
                  selectInput(inputId = "INPUT_yaxis_title",
                              label = "Incidence statistic:",
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
                                    selected = 0.2), 
                  radioGroupButtons(inputId = "INPUT_rollout_modifier",
                                    label = "Rollout speed:",
                                    choices = CHOICES$rollout_modifier,
                                    selected = 1), 
                  radioGroupButtons(inputId = "INPUT_infection_derived_immunity",
                                    label = "Protection from infection-derived immunity:",
                                    choices = CHOICES$infection_derived_immunity,
                                    selected = 1),   
                  radioGroupButtons(inputId = "INPUT_vaccine_derived_immunity",
                                    label = "Protection from vaccine-derived immunity:",
                                    choices = CHOICES$vaccine_derived_immunity,
                                    selected = 1),   
                  
                  h5(strong("Display:")),
                  prettySwitch(
                    label = "heatmap",
                    inputId = "INPUT_display_impact_heatmap",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "date of vaccine availability",
                    inputId = "INPUT_display_vaccine_availability",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "end of essential worker delivery",
                    inputId = "INPUT_display_end_of_essential_worker_delivery",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "colour essential worker delivery",
                    inputId = "INPUT_colour_essential_workers_phase",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  
    ),
    
    
    
    ### Outputs ################################################################
    mainPanel( width = 9,
               
               waiter::useWaiter(),
               
               textOutput("test"),
               tableOutput("test2"),
               
               textOutput("WARNING_no_plot"),
               plotOutput("OUTPUT_plot", height = "800px")
               
    )
  )
)
################################################################################




#### SERVER DEFINITION ########################################################
server <- function(input, output, session) {
  
  # output$test <- renderText({
  #   #"test"
  #   print(c(0,as.numeric(input$INPUT_supply)))
  #   })
  # 
  # output$test2 <- renderTable({
  #   #"test"
  # })
  
  ### Conditional UI components
  output$severe_disease_pt_est_input <- renderUI({
    if(input$this_output != "cases"){
      numericInput(inputId = "TOGGLE_severe_disease_point_estimate", label = "Population-level estimate (%):", value = 1)
    } 
  })
  output$severe_disease_age_dn_input <- renderUI({
    if(input$this_output != "cases"){
      textInput(inputId = "TOGGLE_severe_disease_age_distribution",label = "Age distribution:")
    } 
  })
  output$severe_disease_comorb_increased_risk <- renderUI({
    if(input$this_output != "cases"){
      numericInput(inputId = "TOGGLE_severe_disease_comorb_increased_risk", label = "Increased RR of individuals with comorbidities:", value = 1)
    } 
  })

  
  output$WARNING_no_plot <- renderText({
    check_plot_exists <-    plot_simulations(
      var_1 = input$var_1,
      var_2 = input$var_2,
      yaxis_title = input$INPUT_yaxis_title,
      display_impact_heatmap = input$INPUT_display_impact_heatmap,
      colour_essential_workers_phase = input$INPUT_colour_essential_workers_phase, 
      display_vaccine_availability = input$INPUT_display_vaccine_availability,
      display_end_of_essential_worker_delivery = input$INPUT_display_end_of_essential_worker_delivery,
      load_simulations = FALSE
    )
    if(is.character(check_plot_exists)) {
      validate(paste("\nNote: The underlying simulation for this plot does not exist. There are no simulations available for the selected value of:",check_plot_exists ))
    }
  })
  
  #output plot
  output$OUTPUT_plot <- renderPlot({
    plot_simulations(
      var_1 = input$var_1,
      var_2 = input$var_2,
      yaxis_title = input$INPUT_yaxis_title,
      display_impact_heatmap = input$INPUT_display_impact_heatmap,
      colour_essential_workers_phase = input$INPUT_colour_essential_workers_phase, 
      display_vaccine_availability = input$INPUT_display_vaccine_availability,
      display_end_of_essential_worker_delivery = input$INPUT_display_end_of_essential_worker_delivery,
      load_simulations = FALSE
    )
    
    },
    res = 96)
  
}


shinyApp(ui, server) #run application!