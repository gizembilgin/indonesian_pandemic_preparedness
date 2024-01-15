

load_setting <- function(this_setting = "Indonesia",
                         age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")) {
  
  ### IMPORT
  #(1/8) age-structure
  load("01_inputs/population_MASTER.Rdata")
  if (! this_setting %in% unique(population_MASTER$name_english) || ! this_setting %in% unique(population_MASTER$name_indonesian)){  
    stop("this_setting does not exist in population_MASTER")
  }
  population <- population_MASTER %>%
    filter(name_english == this_setting | name_indonesian == this_setting) %>%
    select(age_group,individuals)
  rm(population_MASTER)
  
  
  #(2/8) contact patterns
  load("01_inputs/contact_matrix_MASTER.Rdata")
  contact_matrix <- contact_matrix_MASTER %>%
    filter(name_english == this_setting | name_indonesian == this_setting) %>%
    select(age_of_individual, age_of_contact, contacts)
  rm(contact_matrix_MASTER)
  if (nrow(contact_matrix) != length(age_group_labels)^2) stop("too many contact matrix rows!")
  
  # simplify tidy contact matrix to matrix form
  workshop <- contact_matrix %>%
    pivot_wider(names_from = age_of_contact,
                values_from = contacts,
                names_prefix = "age_contact_")
  colnames(workshop) <- gsub(" ","_",colnames(workshop))
  workshop$age_of_individual <- factor(workshop$age_of_individual, levels = age_group_labels)
  workshop <- workshop %>% 
    select(age_of_individual, age_contact_0_to_4, age_contact_5_to_17, age_contact_18_to_29, age_contact_30_to_59, age_contact_60_to_110) %>%
    arrange(age_of_individual)
  contact_matrix <- as.matrix(workshop[,-c(1)])
  
  
  #(3/8) % of essential workers (DUMMY VALUE) - COVID-19 vaccination data
  essential_workers <- data.frame(age_group = age_group_labels,
                                  proportion = c(0,rep(0.1,length(age_group_labels)-2),0))
  
  
  #(4/8) daily_vaccine_delivery_capacity (DUMMY VALUE)  - COVID-19 vaccination data
  daily_vaccine_delivery_capacity = 0.00163*sum(population$individuals)
  
  
  #(5/8) vaccine_acceptance (DUMMY VALUE) - COVID-19 vaccination data
  vaccine_acceptance = data.frame(phase = c(rep("essential workers",2),rep("vaccination strategy",2)),
                                  comorbidity = rep(c(0,1),2),
                                  uptake = c(0.95,0.95,0.9,0.95))
  
  
  #(6/8) % comorb (DUMMY VALUE) - Basic Health Survey
  comorbidities <- data.frame(age_group = age_group_labels) %>%
    mutate(proportion = (row_number()-1)*1/length(age_group_labels))
  
  
  #(7/8) access to care (DUMMY VALUE) - Basic Health Survey, seroprevalence, OR health facilities research
  access_to_care <- crossing(age_group = age_group_labels,
                             proportion = 0.8)
  
  
  #(8/8) modification on R0 based on province (DUMMY) - seroprevalence survey
  R0_adjustement = 1
  
  
  #derive population_risk_group
  population_by_comorbidity <- crossing(age_group = age_group_labels,
                                       comorbidity = c(0, 1)) %>%
    left_join(population, by = "age_group") %>%
    left_join(comorbidities, by = "age_group") %>%
    mutate(
      individuals =
        case_when(
          comorbidity == 0 ~ (1 - proportion) * individuals,
          comorbidity == 1 ~ proportion * individuals
        )
    ) %>%
    select(-proportion)
  if (abs(sum(population_by_comorbidity$individuals) - sum(population$individuals))>0) stop("population_by_comorbidity != population")
  

  loaded_setting_characteristics <- list(population = population,
                                         population_by_comorbidity = population_by_comorbidity,
                                         contact_matrix = contact_matrix,
                                         essential_workers = essential_workers,
                                         daily_vaccine_delivery_capacity = daily_vaccine_delivery_capacity,
                                         vaccine_acceptance = vaccine_acceptance,
                                         comorbidities = comorbidities,
                                         access_to_care = access_to_care,
                                         R0_adjustement = R0_adjustement)
  
  return(loaded_setting_characteristics)
  
}