library(DBI)
install.packages("RPostgreSQL")
library(RPostgreSQL)
library(glue)

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

# truncate the table if necessary (only execute if necessary)
tryCatch({
  dbBegin(con)
  
  dbExecute(con, "TRUNCATE TABLE cds_cdm_02.target_cohort CASCADE;")

  dbCommit(con)
}, error = function(e) {
  dbRollback(con)
  ParallelLogger::logError("Error truncating tables:", conditionMessage(e))
})
#---------------------------------------------------------------- Outcome Cohorts Definition

# SQL Queries

# Outcome Cohort 1: get all patients with CR1 = 1
sql_query_cohort_1 <- "
  SELECT person_id
  FROM cds_cdm_02.observation
  WHERE observation_source_value = 'CR1'
  AND value_as_number = 1;
"
result_cohort_1 <- dbGetQuery(con, sql_query_cohort_1)

# Outcome Cohort 2: get all patients who survived over two years
sql_query_cohort_2 <- "
  SELECT person_id
  FROM cds_cdm_02.observation
  WHERE observation_source_value = 'OSSTAT2Years'
  AND value_as_number = 1;
"
result_cohort_2 <- dbGetQuery(con, sql_query_cohort_2)

# Target population: get all patients with CR1 = 1 and OS2Years = 1
sql_query_target_population <- "
  SELECT person_id
  FROM cds_cdm_02.observation
  WHERE observation_source_value IN ('CR1', 'OSSTAT2Years');
"
result_target_population <- dbGetQuery(con, sql_query_target_population)

# Function to insert cohort
insert_cohort <- function(con, result_data_frame, cohort_definition_id) {
  distinct_person_ids <- unique(result_data_frame$person_id)
  
  result_data_frame <- data.frame(
    cohort_definition_id = rep(cohort_definition_id, length(distinct_person_ids)),
    subject_id = distinct_person_ids,
    cohort_start_date = as.Date(rep("1800-01-01", length(distinct_person_ids))),
    cohort_end_date = as.Date(rep(Sys.Date(), length(distinct_person_ids)))
  )
  
  for (i in 1:nrow(result_data_frame)) {
    subject_id <- result_data_frame$subject_id[i]
    cohort_start_date <- result_data_frame$cohort_start_date[i]
    cohort_end_date <- result_data_frame$cohort_end_date[i]
    
    insert_cohort_query <- glue::glue("
      INSERT INTO cds_cdm_02.target_cohort (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
      VALUES ({cohort_definition_id}, {subject_id}, '{cohort_start_date}'::date, '{cohort_end_date}'::date);
    ")
    
    dbExecute(con, as.character(insert_cohort_query))
  }
}

# Call the functions
insert_cohort(con, result_cohort_1, 1)
insert_cohort(con, result_cohort_2, 2)
insert_cohort(con, result_target_population, 3)







