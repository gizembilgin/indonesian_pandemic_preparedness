
estimate_days_to_detection <- function (
    outcome = "deaths",
    outcome_threshold = 2,                              # threshold number of this outcome for detection
    IR_outcome = TOGGLES_project_deaths$point_estimate, # incidence rate for this outcome
    gen_interval = 7,                                   # generation interval (days)
    develop_outcome = 14,                               # time to developing outcome (days)
    R0 = TOGGLE_R0_to_fit                               # basic reproduction number
) {
  
  outcome_threshold = as.numeric(outcome_threshold)
  gen_interval = as.numeric(gen_interval)
  IR_outcome = as.numeric(IR_outcome)
  develop_outcome = as.numeric(develop_outcome)
  R0 = as.numeric(R0)
  
  if(outcome == "presentations") IR_outcome = 0.385 # from 01_inputs/mech_shops/importing_presentations_to_care.R
  
  round((log(outcome_threshold/IR_outcome, base = R0) + 1)*gen_interval + develop_outcome)
  
}

# application to wild-type COVID-19
# estimate_days_to_detection(outcome_threshold = 2, 
#                   gen_interval = 5,      
#                   IR_outcome = 0.00107,       
#                   develop_outcome = 26,  
#                   R0 = 2.79)
# = 68 days

# application to Ebola
# estimate_days_to_detection(outcome_threshold = 2, 
#                   gen_interval = 15,      
#                   IR_outcome = 0.377,       
#                   develop_outcome = 8,  
#                   R0 = 1.95)
# = 60 days


# estimate_days_to_detection(
#   outcome = "presentations",
#   outcome_threshold = 10,
#   gen_interval = 5,
#   IR_outcome = 0.00107,
#   develop_outcome = 26,
#   R0 = 2.79)
# = 47 days if 10 presentations rather than 10 deaths