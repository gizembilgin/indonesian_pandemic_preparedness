
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
  vaccination_strategies = unique(ship_log$phase[! ship_log$phase %in% c("no vaccine", "essential workers")]),
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
                  
                  uiOutput("var_1_input"),
                  uiOutput("var_2_input"),
                  selectInput(inputId = "yaxis_title",
                              label = "Incidence statistic:",
                              choices = CHOICES$incidence_statistic,
                              selected = "incidence"),
                  selectInput(inputId = "this_output",
                              label = "Outcome:",
                              choices = c("cases","deaths","presentations")),
                  selectInput(inputId = "vaccination_strategies",label = "Vaccination strategies:", 
                              choices = CHOICES$vaccination_strategies, selected = "all adults at the same time", multiple = TRUE),
                  
                  #TOGGLES_project_severe_disease
                  uiOutput("TOGGLES_project_severe_disease_point_estimate"),
                  uiOutput("TOGGLES_project_severe_disease_age_distribution"),
                  uiOutput("TOGGLES_project_severe_disease_VE"),
                  uiOutput("TOGGLES_project_comorb_increased_risk"),
                  
                  uiOutput("ui_R0"),
                  uiOutput("ui_vaccine_delivery_start_date"),
                  uiOutput("ui_supply"),
                  uiOutput("ui_rollout_modifier"),
                  uiOutput("ui_infection_derived_immunity"),
                  uiOutput("ui_vaccine_derived_immunity"),

                  h5(strong("Display:")),
                  prettySwitch(
                    label = "free y-axis",
                    inputId = "free_yaxis",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "heatmap",
                    inputId = "display_impact_heatmap",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "severity curve",
                    inputId = "display_severity_curve",
                    value = FALSE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "date of vaccine availability",
                    inputId = "display_vaccine_availability",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "end of essential worker delivery",
                    inputId = "display_end_of_essential_worker_delivery",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  prettySwitch(
                    label = "colour essential worker delivery",
                    inputId = "colour_essential_workers_phase",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  ),
                  
    ),
    
    
    
    ### Outputs ################################################################
    mainPanel( width = 9,
               
               waiter::useWaiter(),
               
               verbatimTextOutput ("test"),
               textOutput("test2"),
               
               textOutput("WARNING_no_plot"),
               plotOutput("OUTPUT_plot", height = "800px")
               
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
 #   input$vaccination_strategies
 # })
  
  
  ### Conditional UI components
 output$var_1_input <- renderUI({
   if(input$this_output != "cases") choices_list = c("pathogen",CHOICES$variable) else choices_list = CHOICES$variable
   selectInput(inputId = "var_1",
               label = "Variable to vary:",
               choices = choices_list,
               selected = "R0")
 })
  output$var_2_input <- renderUI({
    selectInput(inputId = "var_2",
              label = "Second variable to vary (optional):",
              choices = c(CHOICES$variable[CHOICES$variable != input$var_1],"none"),
              selected = "none")
  })
  
  
  output$TOGGLES_project_severe_disease_point_estimate <- renderUI({
    if(input$this_output != "cases") numericInput(inputId = "severe_disease_point_estimate", label = "Population-level estimate (%):", value = 0.01)
  })
  output$TOGGLES_project_severe_disease_age_distribution <- renderUI({
    if(input$this_output != "cases") selectInput(inputId = "severe_disease_age_distribution",label = "Age distribution:", 
                                                 choices = unique(age_specific_severity_MASTER$pathogen), selected = "Plague", multiple = TRUE)
  })
  output$TOGGLES_project_severe_disease_VE <- renderUI({
    if(input$this_output != "cases") numericInput(inputId = "severe_disease_VE", label = "Vaccine effectiveness against severe disease:", value = 1)
  })
  # output$TOGGLES_project_comorb_increased_risk <- renderUI({
  #   if(input$this_output != "cases")  numericInput(inputId = "comorb_increased_risk", label = "Increased RR of individuals with comorbidities:", value = 1)
  # })
  
  
  output$ui_R0 <- renderUI({
    if(is.null(input$var_1) == FALSE && input$var_1 != "R0"){
      radioGroupButtons(inputId = "R0",
                        label = "Basic reproduction number:",
                        choices = CHOICES$R0,
                        selected = 2)
    }  else{
      checkboxGroupButtons(inputId = "R0",
                        label = "Basic reproduction number:",
                        choices = CHOICES$R0,
                        selected = 2)
    }
  })
  output$ui_vaccine_delivery_start_date <- renderUI({
    if(is.null(input$var_1) == FALSE && input$var_1 != "vaccine_delivery_start_date"){
      radioGroupButtons(inputId = "vaccine_delivery_start_date",
                        label = "Days between pathogen detected and vaccine first delivered:",
                        choices = CHOICES$vaccine_delivery_start_date,
                        selected = 100)
    }  else{
      checkboxGroupButtons(inputId = "vaccine_delivery_start_date",
                           label = "Days between pathogen detected and vaccine first delivered:",
                           choices = CHOICES$vaccine_delivery_start_date,
                           selected = 100)
    }
  })
  output$ui_supply <- renderUI({
    if(is.null(input$var_1) == FALSE && input$var_1 != "supply"){
      radioGroupButtons(inputId = "supply",
                        label = "Vaccine supply (% population):",
                        choices = CHOICES$supply,
                        selected = 0.2)
    } else{
      checkboxGroupButtons(inputId = "supply",
                           label = "Vaccine supply (% population):",
                           choices = CHOICES$supply,
                           selected = 0.2)
    }
  })
  output$ui_rollout_modifier <- renderUI({
    if(is.null(input$var_1) == FALSE && input$var_1 != "rollout_modifier"){
      radioGroupButtons(inputId = "rollout_modifier",
                        label = "Rollout speed:",
                        choices = CHOICES$rollout_modifier,
                        selected = 1) 
    } else{
      checkboxGroupButtons(inputId = "rollout_modifier",
                           label = "Rollout speed:",
                           choices = CHOICES$rollout_modifier,
                           selected = 1) 
    }
  })
  output$ui_infection_derived_immunity <- renderUI({
    if(is.null(input$var_1) == FALSE && input$var_1 != "infection_derived_immunity"){
      radioGroupButtons(inputId = "infection_derived_immunity",
                        label = "Protection from infection-derived immunity:",
                        choices = CHOICES$infection_derived_immunity,
                        selected = 1) 
    } else{
      checkboxGroupButtons(inputId = "infection_derived_immunity",
                           label = "Protection from infection-derived immunity:",
                           choices = CHOICES$infection_derived_immunity,
                           selected = 1) 
    }
  })
  output$ui_vaccine_derived_immunity <- renderUI({
    if(is.null(input$var_1) == FALSE && input$var_1 != "vaccine_derived_immunity"){
      radioGroupButtons(inputId = "vaccine_derived_immunity",
                        label = "Protection from vaccine-derived immunity:",
                        choices = CHOICES$vaccine_derived_immunity,
                        selected = 1)
    } else{
      checkboxGroupButtons(inputId = "vaccine_derived_immunity",
                           label = "Protection from vaccine-derived immunity:",
                           choices = CHOICES$vaccine_derived_immunity,
                           selected = 1)
    }
  })
  
  # this_var_1_range <- reactive({
  #   if (input$var_1 == "R0"){input$R0} else{0}
  # })

  
  # output$WARNING_no_plot <- renderText({
  #   check_plot_exists <-    plot_simulations(
  #     var_1 = input$var_1,
  #     var_2 = var_2_reactive,
  #     yaxis_title = input$yaxis_title,
  #     display_impact_heatmap = input$display_impact_heatmap,
  #     colour_essential_workers_phase = input$colour_essential_workers_phase, 
  #     display_vaccine_availability = input$display_vaccine_availability,
  #     display_end_of_essential_worker_delivery = input$display_end_of_essential_worker_delivery,
  #     load_simulations = FALSE
  #   )
  #   if(is.character(check_plot_exists)) {
  #     validate(paste("\nNote: The underlying simulation for this plot does not exist. There are no simulations available for the selected value of:",check_plot_exists ))
  #   }
  # })
  # 
  #output plot
  
  
  output$OUTPUT_plot <- renderPlot({
    
    if (is.null(input$var_1)== FALSE){
      if (input$var_1 == "R0"){this_var_1_range = input$R0
      } else if (input$var_1 == "vaccine_delivery_start_date"){ this_var_1_range = input$vaccine_delivery_start_date
      } else if (input$var_1 == "supply"){ this_var_1_range = input$supply
      } else if (input$var_1 == "infection_derived_immunity"){ this_var_1_range = input$infection_derived_immunity
      } else if (input$var_1 == "rollout_modifier"){ this_var_1_range = input$rollout_modifier
      } else if (input$var_1 == "vaccine_derived_immunity"){ this_var_1_range = input$vaccine_derived_immunity
      } else{this_var_1_range = NA}
    } else{this_var_1_range = NA}
    
    if ((input$this_output == "cases" |
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
      
      plot_simulations(
        var_1 = input$var_1,
        var_2 = input$var_2,
        yaxis_title = input$yaxis_title,
        this_output = input$this_output,
        TOGGLES_project_severe_disease = list(
          point_estimate =  input$severe_disease_point_estimate,
          age_distribution = input$severe_disease_age_distribution,
          VE_severe_disease =  input$severe_disease_VE,
          comorb_increased_risk = 1
        ), 
        var_1_range = this_var_1_range,
        default_configuration =
          list(
            R0 = input$R0,
            vaccine_delivery_start_date = as.numeric(input$vaccine_delivery_start_date),
            phase = c(input$vaccination_strategies,"essential workers", "no vaccine"),
            supply = as.numeric(input$supply),
            infection_derived_immunity =  as.numeric(input$infection_derived_immunity),
            rollout_modifier =  as.numeric(input$rollout_modifier),
            vaccine_derived_immunity =  as.numeric(input$vaccine_derived_immunity)
          ),
        free_yaxis = input$free_yaxis,
        display_impact_heatmap = input$display_impact_heatmap,
        display_severity_curve = input$display_severity_curve,
        display_var_1 = 0,
        colour_essential_workers_phase = input$colour_essential_workers_phase,
        display_vaccine_availability = input$display_vaccine_availability,
        display_end_of_essential_worker_delivery = input$display_end_of_essential_worker_delivery,
        load_simulations = FALSE
      )
    }

  },
  res = 96)
  
}


shinyApp(ui, server) #run application!