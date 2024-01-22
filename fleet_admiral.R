
### The 'fleet admiral' runs the command_deck  multiple times to generate the data set underlying the Shiny 

### CONFIGURE
################################################################################
#scenario configuration
LIST_setting = c("Indonesia")
LIST_vaccine_delivery_start_date = c(50,100)

#pathogen characteristics
LIST_R0_to_fit = c(1,2,4,6,8)  #  c(1,2,4,6,8) 
#LIST_severity_scenario
#LIST_prior_immunity_scenario
LIST_infection_derived_immunity = c(0.75,1)

#vaccination strategies
LIST_rollout_modifier = c(0.5,1,2)
LIST_vaccine_derived_immunity = c(0.75,1)
LIST_supply = c(0.2,0.5,0.8)
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
            #OPTION: to save in a different format, e.g., JSON to save memory BUT requires more complex format
            
            ship_log = rbind(ship_log,this_simulation)
            
          }
        }
      }
    }
  }
}

time_of_result = Sys.time()
time_of_result = gsub(':','-',time_of_result)
save(ship_log,file = paste0("04_shiny/x_results/ship_log",time_of_result,".Rdata"))

rm(FLEET_ADMIRAL_OVERRIDE)
#_______________________________________________________________________________


### MANIPULATE DATA
################################################################################
### reconstruct complete incidence from cascade of simulation

#first let's collapse to daily incidence
workshop <- ship_log %>%
  group_by(time,phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>%
  summarise(incidence = sum(incidence), .groups = "keep")  %>%
  mutate(flag_reconstructed = 0) #NB: include as flag so option to not include in incidence plot

additional_rows = data.frame()
for (this_vaccine_delivery_start_date in LIST_vaccine_delivery_start_date){
  for (this_rollout_modifier in LIST_rollout_modifier){
    
    this_workshop <- workshop %>% 
      filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date &
               rollout_modifier == this_rollout_modifier &
               ! phase %in% c("no vaccine","essential workers"))
    
    #Part 1/2: no vaccine and essential worker phases
    this_before_strategy = workshop %>% 
      filter(vaccine_delivery_start_date == this_vaccine_delivery_start_date &
               rollout_modifier == this_rollout_modifier) %>%
      filter((phase == "no vaccine" & time < vaccine_delivery_start_date) |
               phase %in% c("essential workers")) %>% #CHECKED: unique(to_propogate$supply) == 0 for no vax and 0.2 for essential workers
      ungroup() %>%
      select(-phase,-supply)
    before_strategy_contribution = this_workshop %>%
      ungroup() %>%
      select(phase,supply) %>%
      unique() %>%
      crossing(this_before_strategy) %>%
      mutate(flag_reconstructed = 1)
    additional_rows = rbind(additional_rows,before_strategy_contribution)
    
    #Part 2/2: strategy specific phases - cascade!
    supply_loop = unique(this_workshop$supply)
    supply_loop = supply_loop[supply_loop>0]
    supply_loop = as.numeric(sort(supply_loop))
  
    for (this_supply in supply_loop[supply_loop != max(supply_loop)]){
    
    next_supply = supply_loop[which(supply_loop == this_supply)+1]
    next_supply_times = this_workshop %>% 
      filter(supply == next_supply) 
    check = next_supply_times %>% 
      group_by(time,phase) %>% 
      summarise(n=n(), .groups = "keep") %>%
      ungroup() %>%
      select(n) %>%
      unique()
    if (nrow(check)>1){stop("fleet_admiral: next_supply_times has unequal entries across phases")}
    next_supply_times = next_supply_times %>%
      ungroup() %>%
      select(time) %>%
      unique()
    
    cascade_contribution <- this_workshop %>%
      filter(supply == this_supply &
               phase != "essential workers" &
               !(time %in% next_supply_times$time)) %>%
      ungroup() %>%
      select(-supply) %>%
      crossing(supply = supply_loop[supply_loop > this_supply])  %>%
      mutate(flag_reconstructed = 1)
    
    additional_rows = rbind(additional_rows,cascade_contribution)
  
    }
  }
}

#Combine and check
ship_log_completed = rbind(workshop, additional_rows)

object.size(ship_log_completed)/object.size(workshop) #= 1.8
nrow(ship_log_completed)/nrow(workshop) #= 1.8

check = ship_log_completed %>%
  filter(! phase %in% c("essential workers", "no vaccine")) %>% 
  group_by(phase,supply,setting,vaccine_delivery_start_date,R0,infection_derived_immunity,rollout_modifier,vaccine_derived_immunity) %>% 
  summarise(n=n(), .groups = "keep") %>%
  filter(n != TOGGLE_time_horizon)
if (nrow(check)>1){stop("fleet_admiral: not all phase-supply-etc. scenarios have 365 days in ship_log_completed")}
rm(workshop,cascade_contribution,additional_rows,this_before_strategy,before_strategy_contribution, this_workshop)

save(ship_log_completed,file = paste0("04_shiny/x_results/ship_log_completed",time_of_result,".Rdata"))

###
#require(parallel); require(foreach)
# CLUSTER <- parallel::makeCluster(TOGGLE_clusterNumber) # create cluster
# doParallel::registerDoParallel(CLUSTER) # activate cluster
# 
# CommandDeck_result_long <- foreach::foreach(
#   model_run_number = c(1:TOGGLE_clusterNumber),
#   .packages = c("tidyverse","ids"),
#   .combine = rbind,
#   .inorder = FALSE
# )  %dopar% {
#   
#   CEA_worker(
#     numberOfRunsPerCluster,
#     CEA_risk_group,
#     LIST_CEA_settings,
#     LIST_perspectives,
#     LIST_booster_vax_scenarios,
#     LIST_antiviral_elig_groups,
#     LIST_antiviral_types,
#     DECISION_sampling_strategy,
#     DECISION_include_net,
#     TOGGLE_uncertainty,
#     TOGGLE_longCOVID,
#     LIST_discounting_rate,
#     LIST_antiviral_cost_scenario,
#     TORNADO_PLOT_OVERRIDE
#   )
#   
# }
# })
# 
# parallel::stopCluster(CLUSTER)