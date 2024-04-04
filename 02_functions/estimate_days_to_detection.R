
estimate_days_to_detection <- function (
    outcome_threshold, # threshold number of this outcome for detection
    gen_interval,      # generation interval (days)
    IR_outcome,        # incidence rate for this outcome
    develop_outcome,   # time to developing outcome (days)
    R0                 # basic reproduction number
) {
  (log(outcome_threshold/IR_outcome, base = R0) + 1)*gen_interval + develop_outcome
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