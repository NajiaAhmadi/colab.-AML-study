if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")

#install.packages(c("DatabaseConnector", "SqlRender"))
library(DatabaseConnector)
library(SqlRender)
library(readxl)
library(glue)
library(RPostgres)
library(tidyr)
library(dplyr)


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

#---------------------------------- Data and Mappings

input_data <- read.csv("20240116_sal_ohsu_data_OS2.csv")
mapping <- read_excel("20240122_Mappings_sal.xlsx", sheet = "Mappings")

#--------------------------- get the big number values for measurement table

selected_mapping_observation_nonBinary <- subset(mapping, mapping$"Transformation_name" == "Observation_bigNumber")
listcolnames_observation_nonBinary <- selected_mapping_observation_nonBinary$Dataset_column

missing_cols <- setdiff(c("Pat", listcolnames_observation_nonBinary), colnames(input_data))

if (length(missing_cols) == 0) {
  
  # Select specific columns from input_data based on listcolnames_observation_nonBinary
  selected_columns <- input_data[, c("Pat", listcolnames_observation_nonBinary), drop = FALSE]
  
  # Use tidyr::pivot_longer to transform to a vertical table
  vertical_table_observation_nonBinary <- tidyr::pivot_longer(
    selected_columns,
    cols = -Pat,
    names_to = "Variable",
    values_to = "Value"
  )
  
  # Print the resulting vertical table
  print(vertical_table_observation_nonBinary)
  
} else {
  cat("Error: The following specified columns do not exist in input_data:", missing_cols, "\n")
}

# adding the UNITs as well to the dataset
vertical_table_observation_nonBinary <- vertical_table_observation_nonBinary %>%
  mutate(
    unit = case_when(
      Variable == "HB" ~ "mmol/l",
      Variable == "OSTM" ~ "Months",
      Variable == "EFSTM" ~ "Months",
      Variable == "RFSTM" ~ "Mo",
      TRUE ~ NA_character_
    )
  )


# adding concept ids for UNITs
vertical_table_observation_nonBinary <- vertical_table_observation_nonBinary %>%
  mutate(
    unit_concept_id = case_when(
      unit == "mmol/l" ~ "8753",
      unit == "Months" ~ "9580",
      unit == "Mo" ~ "9580",
      TRUE ~ NA_character_
    )
  )

# get the concept ids for the concepts 
vertical_table_observation_nonBinary$Concept_id <- NA

for (i in seq(nrow(vertical_table_observation_nonBinary))) {
  current_variable <- vertical_table_observation_nonBinary$Variable[i]
  matching_concept_id <- mapping$Concept_id[mapping$Dataset_column == current_variable]
  vertical_table_observation_nonBinary$Concept_id[i] <- ifelse(length(matching_concept_id) > 0, matching_concept_id[1], NA)
}

print(vertical_table_observation_nonBinary)

# Check for NAs in specific columns (e.g., Concept_id, Variable, Value)
columns_to_check <- c("Concept_id", "Variable", "Value", "unit", "unit_concept_id")

for (col in columns_to_check) {
  if (any(is.na(vertical_table_observation_nonBinary[[col]]))) {
    cat("There are NAs in column", col, "of the vertical_table_observation_nonBinary\n")
  } else {
    cat("No NAs found in column", col, "of the vertical_table_observation_nonBinary\n")
  }
}

# remove NA entries in the value colum

vertical_table_observation_nonBinary <- na.omit(vertical_table_observation_nonBinary, cols = c("Value"))

# Print the resulting vertical table
print(vertical_table_observation_nonBinary)

# query the patient ids from person table
get_person_id <- function(con, patient_id) {
  query <- glue::glue("
    SELECT person_id FROM cds_cdm.person WHERE person_source_value = '{patient_id}';
  ")
  result <- dbGetQuery(con, as.character(query))
  
  if (nrow(result) == 0) {
    warning(paste("No matching person_id found for patient:", patient_id))
    return(NA)
  }
  
  return(result$person_id)
}


for (i in 1:nrow(vertical_table_observation_nonBinary)) {
  
  person_id <- get_person_id(con, vertical_table_observation_nonBinary$Pat[i])
  observation_concept_id <- vertical_table_observation_nonBinary$Concept_id[i]
  observation_date <- "01.01.1800"
  observation_type_concept_id <- "32817"
  observation_source_value <- vertical_table_observation_nonBinary$Variable[i]
  value_as_number <- vertical_table_observation_nonBinary$Value[i]
  unit_source_value <- vertical_table_observation_nonBinary$unit[i]
  unit_concept_id <- vertical_table_observation_nonBinary$unit_concept_id[i]
  
  query <- glue::glue("
    INSERT INTO cds_cdm.observation (
      person_id, observation_concept_id, observation_date, 
      observation_type_concept_id, observation_source_value,
      value_as_number, unit_source_value, unit_concept_id
    )
    VALUES (
      {person_id}, {observation_concept_id}, '{observation_date}',
      '{observation_type_concept_id}','{observation_source_value}', 
      {value_as_number}, '{unit_source_value}', '{unit_concept_id}'
    );
  ")
  dbExecute(con, as.character(query))
}

dbDisconnect(con)
cat("Measurement data inserted into cds_cdm.observation table.\n")











