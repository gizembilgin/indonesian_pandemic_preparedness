
### TIME one run
system.time({source("command_deck.R")})
# user  system elapsed 
# 43.33    0.81   45.01 

### PROFILE it
profvis::profvis({source("command_deck.R")})



