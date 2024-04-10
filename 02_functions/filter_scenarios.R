### Apply configuration to subset data to desired scenario
filter_scenarios <- function(data,configuration,warning_search = 0){
  for (i in 1:length(configuration)){
    if (names(configuration)[i] == "supply"){
      data = data %>% 
        filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]] | 
                 phase %in% c("no vaccine", "healthcare workers"))
    } else if (names(configuration[i]) == "days_to_detection"){
      data = data %>%
        right_join(configuration[[i]], by = join_by(R0, days_to_detection))
    } else{
      data = data %>% 
        filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]]) 
    }
    if (nrow(data) == 0) stop(paste("filter_scenarios: There are no simulations using",names(configuration)[[i]],"=",configuration[[i]]))
    if (warning_search == 1 & nrow(data[data$supply != 0,]) == 0){
      return(names(configuration)[[i]])
    }
  }
  return(data)
}
#____________________________________________________________