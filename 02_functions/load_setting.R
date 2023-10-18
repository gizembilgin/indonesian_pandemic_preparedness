

load_setting <- function(this_setting = "Indonesia") {
  
  load("01_inputs/population_MASTER.Rdata")
  
  #CHECK inputs
  if (! this_setting %in% unique(population_MASTER$name_english) || ! this_setting %in% unique(population_MASTER$name_indonesian)){  
      stop("this_setting does not exist in population_MASTER")
  }

  pop <- population_MASTER %>%
    filter(name_english == this_setting | name_indonesian == this_setting) %>%
    select(age_group,individuals)
  rm(population_MASTER)
}