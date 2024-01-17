
### The 'fleet admiral' runs the command_deck  multiple times to generate the data set underlying the Shiny 

### CONFIGURE
################################################################################
#scenario configuration
LIST_setting = c("Indonesia")
LIST_vaccine_delivery_start_date = c(50,100)

#pathogen characteristics
LIST_R0_to_fit = c(1,2,4,6,8)
#LIST_severity_scenario
#LIST_prior_immunity_scenario
LIST_infection_derived_immunity = c(0.75,1)

#vaccination strategies
LIST_supply = c(0.2,0.5,0.8)
LIST_rollout_modifier = c(0.5,1,2)
LIST_strategy = list(
  #list all strategies as individual lists (c(age groups), c(comorbidity status where 1 = has a comorbidity))
  list("older adults followed by all adults",
       list(c("60 to 110"),c(0,1)),
       list(c("18 to 29","30 to 59"),c(0,1))),
  list("all adults at the same time",
       list(c("18 to 29","30 to 59","60 to 110"),c(0,1))),
  list("children before adults", 
       list(c("0 to 4","5 to 17"),c(0,1)), 
       list(c("18 to 29","30 to 59","60 to 110"),c(0,1)))                                                     
  )
LIST_vaccine_derived_immunity = c(0.75,1)
#_______________________________________________________________________________


### RUN
################################################################################
length(LIST_setting)*length(LIST_vaccine_delivery_start_date)*
  length(LIST_R0_to_fit)*length(LIST_infection_derived_immunity)*
  length(LIST_rollout_modifier)*length(LIST_vaccine_derived_immunity)

ship_log <- data.frame()

for (setting in LIST_setting){
  for (vaccine_delivery_start_date in LIST_vaccine_delivery_start_date){
    for (R0_to_fit in LIST_R0_to_fit){
      for(infection_derived_immunity in LIST_infection_derived_immunity){
        for (rollout_modifier in LIST_rollout_modifier){
          for (vaccine_derived_immunity in LIST_vaccine_derived_immunity){
            
            FLEET_ADMIRAL_OVERRIDE = list(
              setting = setting,
              vaccine_delivery_start_date = vaccine_delivery_start_date,
              
              R0 = R0_to_fit,
              infection_derived_immunity = infection_derived_immunity,
              
              supply = LIST_supply,
              rollout_modifier = rollout_modifier,
              strategy = LIST_strategy,
              vaccine_derived_immunity = vaccine_derived_immunity
            )
            
            source("command_deck.R")
            
            this_simulation <- incidence_log_tidy %>%
              mutate(setting = FLEET_ADMIRAL_OVERRIDE$setting,
                     vaccine_delivery_start_date = FLEET_ADMIRAL_OVERRIDE$vaccine_delivery_start_date,
                     R0 = FLEET_ADMIRAL_OVERRIDE$R0,
                     infection_derived_immunity = FLEET_ADMIRAL_OVERRIDE$infection_derived_immunity,
                     rollout_modifier = FLEET_ADMIRAL_OVERRIDE$rollout_modifier,
                     vaccine_derived_immunity = FLEET_ADMIRAL_OVERRIDE$vaccine_derived_immunity)
            #NB: object.size(this_simulation)/object.size(incidence_log_tidy) -> x 2 size
            
            ship_log = rbind(ship_log,this_simulation)
            
          }
        }
      }
    }
  }
}

rm(FLEET_ADMIRAL_OVERRIDE)
#_______________________________________________________________________________