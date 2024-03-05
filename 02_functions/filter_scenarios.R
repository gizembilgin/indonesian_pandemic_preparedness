### Apply configuration to subset data to desired scenario
filter_scenarios <- function(data,configuration,warning_search = 0){
  for (i in 1:length(configuration)){
    if (names(configuration)[i] == "supply"){
      data = data %>% 
        filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]] | 
                 phase %in% c("no vaccine", "essential workers"))
    } else{
      data = data %>% 
        filter(.data[[names(configuration)[[i]]]] %in% configuration[[i]]) 
    }
    if (warning_search == 1 & nrow(data[data$supply != 0,]) == 0){
      return(names(configuration)[[i]])
    }
  }
  return(data)
}
#____________________________________________________________