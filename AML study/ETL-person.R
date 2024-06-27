if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")

#install.packages(c("DatabaseConnector", "SqlRender"))
library(DatabaseConnector)
library(SqlRender)
library(readxl)
library(glue)
library(RPostgres)
library(tidyr)

source("config.R")

# Database Connection
tryCatch({
  ParallelLogger::logInfo("Connecting to the database...")
  
  con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db,
                   port = db_port, user = db_user, password = db_password)
  
  ParallelLogger::logInfo("Connected to the database successfully.")
}, error = function(e) {
  ParallelLogger::logError("Error connecting to the database:", conditionMessage(e))
  ParallelLogger::logError("Connection details:", db_info)
})


tryCatch({
  dbBegin(con)
  
  dbExecute(con, "TRUNCATE TABLE cds_cdm_02.person;")
  #dbExecute(con, "TRUNCATE TABLE cds_cdm_02.observation_period CASCADE;")
  
  dbCommit(con)
}, error = function(e) {
  dbRollback(con)
  ParallelLogger::logError("Error truncating tables:", conditionMessage(e))
})

#---------------------------------- Data and Mappings

input_data <- read.csv("20240116_sal_ohsu_data_OS2.csv")
mapping <- read_excel("20240122_Mappings_sal.xlsx", sheet = "Mappings")

male_concept_id <- 442985
female_concept_id <- 442986

# Iterate through each row in the mapping document
for (i in 1:nrow(input_data)) {
  
  gender_value <- tolower(input_data$SEX[i])
  gender_concept_id <- ifelse(input_data$SEX[i] == "m", male_concept_id, 
                              ifelse(input_data$SEX[i] == "f", female_concept_id, NA))
  
  age <- input_data$AGE[i]
  patient_id <- input_data$Pat[i]
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  year_of_birth <- current_year - age
  
  insert_person_query <- glue::glue("
  INSERT INTO cds_cdm_02.person (gender_concept_id, gender_source_value, year_of_birth, person_source_value)
  VALUES ({as.numeric(gender_concept_id)}, '{gender_value}', {year_of_birth}, '{patient_id}');
")
  
  dbExecute(con, as.character(insert_person_query))
}

dbDisconnect(con)
cat("Person table populated with patient information.\n")

