

load_setting <- function(this_setting = "Indonesia",
                         age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")) {
  
  ### IMPORT
  #(1/?) age-structure
  load("01_inputs/population_MASTER.Rdata")
  if (! this_setting %in% unique(population_MASTER$name_english) || ! this_setting %in% unique(population_MASTER$name_indonesian)){  
    stop("this_setting does not exist in population_MASTER")
  }
  population <- population_MASTER %>%
    filter(name_english == this_setting | name_indonesian == this_setting) %>%
    select(age_group,individuals)
  rm(population_MASTER)
  
  #(2/?) contact patterns
  load("01_inputs/contact_matrix_MASTER.Rdata")
  contact_matrix <- contact_matrix_MASTER %>%
    filter(name_english == this_setting | name_indonesian == this_setting) %>%
    select(age_of_individual, age_of_contact, contacts)
  rm(contact_matrix_MASTER)
  if (nrow(contact_matrix) != length(age_group_labels)^2){stop("too many contact matrix rows!")}
  
  #(3/?) % of essential workers (DUMMY VALUE) - COVID-19 vaccination data
  essential_workers <- data.frame(age_group = age_group_labels,
                                  proportion = c(0,rep(0.1,length(age_group_labels)-2),0))
  
  #(4/?) % comorb (DUMMY VALUE) - Basic Health Survey
  comorbidities <- data.frame(age_group = age_group_labels) %>%
    mutate(proportion = (row_number()-1)*1/length(age_group_labels))
  
  #(5/?) access to care (DUMMY VALUE) - Basic Health Survey, seroprevalence, OR health facilities research
  access_to_care <- crossing(age_group = age_group_labels,
                             proportion = 0.8)
  
  #(6/?) modification on R0 based on province (DUMMY) - seroprevalence survey
  R0_adjustement = 1
  
  loaded_setting_characteristics <- list(population = population,
                                         contact_matrix = contact_matrix,
                                         essential_workers = essential_workers,
                                         comorbidities = comorbidities,
                                         access_to_care = access_to_care,
                                         R0_adjustement = R0_adjustement)
  
  return(loaded_setting_characteristics)
  
}