library("dplyr")
library("glue")
library("PatientLevelPrediction")
library("DBI")
library("RPostgreSQL")

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

cohort_CR <- tbl(con, dbplyr::in_schema("cds_cdm", "target_cohort" )) %>% 
  rename("person_id" = "subject_id") %>% 
  filter(cohort_definition_id == 1) %>% select("person_id", "cohort_start_date")

person <- tbl(con, dbplyr::in_schema("cds_cdm", "person"))
condition_occurrence <- tbl(con, dbplyr::in_schema("cds_cdm", "condition_occurrence"))
observation <- tbl(con, dbplyr::in_schema("cds_cdm", "observation"))
measurement <- tbl(con, dbplyr::in_schema("cds_cdm", "measurement"))

final_cohort_CR <- cohort_CR %>% left_join(person, by='person_id') %>%
  
  select("gender_concept_id", "person_id") %>% collect() %>% 
  gather(variable, value, -(c(person_id))) %>% 
  mutate(value2 = value)  %>% unite(temp, variable, value2) %>% 
  distinct(.keep_all = TRUE) %>% 
  spread(temp, value) %>% left_join(final_cohort, by="person_id") 


