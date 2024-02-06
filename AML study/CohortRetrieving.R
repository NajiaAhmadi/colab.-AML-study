library(dplyr)
library(dbplyr)
library(tidyr)
library(glue)
library(DBI)
library(RPostgreSQL)
library(data.table)
library(impute)

#-------------------------------------------------------------- Database Connection
if (!file.exists("config.R")) {
  stop("Error: 'config.R' not found in the current directory.")
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("config.R")
tryCatch({
  ParallelLogger::logInfo("Connecting to the database...")
  
  db_info <- paste("user=", db_user, " password=", db_password,
                   " dbname=", db, " host=", host_db, " port=", db_port, sep = ",")
  con <- dbConnect(RPostgres::Postgres(), 
                   dbname = db, 
                   host = host_db,
                   port = db_port, 
                   user = db_user, 
                   password = db_password)
  
  ParallelLogger::logInfo("Connected to the database successfully.")
}, error = function(e) {
  ParallelLogger::logError("Error connecting to the database:", conditionMessage(e))
  ParallelLogger::logError("Connection details:", db_info)
})
#-------------------------------------------------------------- Get the cohort
 
# retrieve relevant omop tables
cohort_CR <- tbl(con, dbplyr::in_schema("cds_cdm", "target_cohort" )) %>% 
  rename("person_id" = "subject_id") %>% 
  filter(cohort_definition_id == 3) %>% 
  select("person_id") %>% collect()

person <- tbl(con, dbplyr::in_schema("cds_cdm", "person")) %>% collect()

final_cohort_CR4 <- cohort_CR %>%
  left_join(person, by = 'person_id') %>% 
  select(-c(#gender_concept_id,
            gender_source_value,
            month_of_birth, 
            day_of_birth, 
            birth_datetime,
            location_id, 
            provider_id,
            care_site_id,
            gender_source_concept_id,
            race_source_value,
            race_source_concept_id,
            ethnicity_source_value,
            ethnicity_source_concept_id,
            ethnicity_concept_id,
            race_concept_id,
            person_source_value)) 

condition_occurrence <- tbl(con, dbplyr::in_schema("cds_cdm", "condition_occurrence")) %>% 
  collect() %>% 
  select(person_id, 
         condition_concept_id,
         #condition_source_value, 
         condition_status_source_value) %>%
  group_by(person_id, condition_concept_id) %>%
  summarise(condition_status_source_value = first(condition_status_source_value, na.rm = TRUE)) %>%
  ungroup()

final_cohort_CR3 <- cohort_CR %>%
  left_join(condition_occurrence, by = 'person_id') %>%
  spread(key = condition_concept_id, value = condition_status_source_value) #%>% select(-"<NA>") 


observation <- tbl(con, dbplyr::in_schema("cds_cdm", "observation")) %>% 
  collect() %>%
  select(person_id, 
         observation_concept_id,
         #observation_source_value, 
         value_as_number) %>% 
  group_by(person_id, observation_concept_id) %>%
  summarise(value_as_number = first(value_as_number, na.rm = TRUE)) %>%
  ungroup()

final_cohort_CR2 <- cohort_CR %>%
  left_join(observation, by = 'person_id') %>%
  spread(key = observation_concept_id, value = value_as_number) 

measurement <- tbl(con, dbplyr::in_schema("cds_cdm", "measurement")) %>% 
  collect() %>%
  select(person_id,
         measurement_concept_id,
         #measurement_source_value, 
         value_source_value) %>%
  group_by(person_id, measurement_concept_id) %>%
  summarise(value_source_value = first(value_source_value, na.rm = TRUE)) %>%
  ungroup()

final_cohort_CR1 <- cohort_CR %>%
  left_join(measurement, by = 'person_id') %>%
  select(person_id, measurement_concept_id, value_source_value) %>%
  spread(key = measurement_concept_id, value = value_source_value) 


# Join tables
final_cohort <- 
  left_join(final_cohort_CR4, final_cohort_CR2, by = "person_id") %>%
  left_join(final_cohort_CR3, by = "person_id") %>%
  left_join(final_cohort_CR1, by = "person_id") %>% select(-"<NA>")

# change column names to type integer
#columns_to_exclude <- c("person_id", "year_of_birth", "gender_source_value")
#columns_to_convert <- setdiff(colnames(final_cohort), columns_to_exclude)
#colnames(final_cohort)[columns_to_convert] <- 
#  as.integer(as.character(colnames(final_cohort)[columns_to_convert]))

final_cohort$`4014046` <- as.integer(final_cohort$`4014046`)
final_cohort$'44804077' <- as.integer(final_cohort$'44804077')

# remove any row for which the CR1 is NA
final_cohort <- final_cohort %>% filter(!is.na(`4014046`))
final_cohort <- final_cohort %>% filter(!is.na(`44804077`))

#impute NAs in the rest columns
library(Hmisc)

custom_impute_function <- function(df) {
  for (col in names(df)) {
    if (class(df[[col]]) == "character") {
      df[[col]][is.na(df[[col]])] <- "2"
    } else if (is.numeric(df[[col]]) || is.double(df[[col]])) {
      # Impute missing values using Hmisc imputation
      df[[col]] <- impute(df[[col]], fun = mean)
    }
    # Add more conditions for other data types if needed
  }
  return(df)
}

# Example usage:
final_cohort <- custom_impute_function(final_cohort)

#check for NAs
sum(is.na(final_cohort))

dim(final_cohort)

# save the cohort
write.csv(final_cohort, "final_cohort.csv", row.names = FALSE)


#column_types <- sapply(final_cohort, class)
# Print the result
#print(column_types)


