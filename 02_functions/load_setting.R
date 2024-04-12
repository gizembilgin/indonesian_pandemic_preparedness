

load_setting <- function(include_comorbidity = FALSE,
                         this_setting = "Indonesia",
                         age_group_labels = c("0 to 4","5 to 17","18 to 29","30 to 59","60 to 110")) {
  
  ### IMPORT
  #(1/8) age-structure
  load(paste0(gsub("/04_shiny","",getwd()),"/01_inputs/population_MASTER.Rdata"))
  if (! this_setting %in% unique(population_MASTER$name_english) || ! this_setting %in% unique(population_MASTER$name_indonesian)){  
    stop("this_setting does not exist in population_MASTER")
  }
  population <- population_MASTER %>%
    filter(name_english == this_setting | name_indonesian == this_setting) %>%
    select(age_group,individuals)
  
  indonesia_population_numeric <- population_MASTER %>%
    filter(name_english == "Indonesia") %>%
    summarise(individuals = sum(individuals))
  indonesia_population_numeric < - indonesia_population_numeric$individuals
  rm(population_MASTER)
  
  
  #(2/8) contact patterns
  load(paste0(gsub("/04_shiny","",getwd()),"/01_inputs/contact_matrix_MASTER.Rdata"))
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
  
  
  #(3/8) % of healthcare workers - COVID-19 vaccination data
  #2 million doses delivered to healthcare workers during the first prioritised stage of the Indonesian COVID-19 vaccine rollout
  #NB: Assuming healthcare workers all 18 to 59, and uniformly distributed
  healthcare_workers <- 2046639/indonesia_population_numeric #% of population
  healthcare_workers <- healthcare_workers * sum(population$individuals) / sum(population$individuals[population$age_group %in%  c("18 to 29","30 to 59")]) #% of 18 to 59 working population
  healthcare_workers <- healthcare_workers$individuals
  healthcare_workers <- data.frame(age_group = age_group_labels,
                                  proportion = c(0,0,healthcare_workers,healthcare_workers,0))
  
  
  #(4/8) daily_vaccine_delivery - COVID-19 vaccination data
  daily_vaccine_delivery <- data.frame(days = c(as.numeric(as.Date('2021-03-01') - as.Date('2021-01-12')),
                                                                            as.numeric(as.Date('2021-06-01') - as.Date('2021-03-01')),
                                                                            as.numeric(as.Date('2022-02-01') - as.Date('2021-06-01'))),
                                                capacity =   c(0.89/as.numeric(as.Date('2021-03-01') - as.Date('2021-01-12')),
                                                                              (7.34-0.89)/as.numeric(as.Date('2021-06-01') - as.Date('2021-03-01')),
                                                                              (80.25-7.34)/as.numeric(as.Date('2022-02-01') - as.Date('2021-06-01')))/100)
  daily_vaccine_delivery$capacity = daily_vaccine_delivery$capacity*sum(population$individuals)
  
  
  #(5/8) vaccine_acceptance (DUMMY VALUE) - COVID-19 vaccination data
  vaccine_acceptance = data.frame(phase = c(rep("healthcare workers",2),rep("vaccination strategy",2)),
                                  comorbidity = rep(c(FALSE,TRUE),2),
                                  uptake = c(0.95,0.95,0.9,0.95))
  
  
  #(6/8) % comorb 
  #NB: currently value from Clark et al. (2020, https://doi.org/10.1016/s2214-109x(20)30264-3) but a 
  #    better estimate could be taken from the  Basic Health Survey 2024 once that is released
  if (include_comorbidity == TRUE){
    load(paste0(gsub("/04_shiny","",getwd()),"/01_inputs/comorbidities_CLARK.Rdata"))
    population <- crossing(age_group = age_group_labels,
                                          comorbidity = c(FALSE,TRUE)) %>%
      left_join(population, by = "age_group") %>%
      left_join(comorbidities, by = "age_group") %>%
      mutate(
        individuals =
          case_when(
            comorbidity == FALSE ~ (1 - proportion) * individuals,
            comorbidity == TRUE ~ proportion * individuals
          )
      ) %>%
      select(-proportion)
    population$age_group <- factor(population$age_group, levels = age_group_labels)
  } else{
    population <- population %>% mutate(comorbidity = FALSE)
  }
  
  
  #(7/8) access to care (DUMMY VALUE) - Basic Health Survey, seroprevalence, OR health facilities research
  access_to_care <- crossing(age_group = age_group_labels,
                             proportion = 0.8)
  
  
  #(8/8) modification on R0 based on province - seroprevalence survey
  R0_adjustement = 1
  
  
  loaded_setting_characteristics <- list(population = population,
                                         contact_matrix = contact_matrix,
                                         healthcare_workers = healthcare_workers,
                                         daily_vaccine_delivery = daily_vaccine_delivery,
                                         vaccine_acceptance = vaccine_acceptance,
                                         access_to_care = access_to_care,
                                         R0_adjustement = R0_adjustement)
  
  return(loaded_setting_characteristics)
  
}