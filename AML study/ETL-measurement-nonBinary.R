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

selected_mapping_Measurement_nonBinary <- subset(mapping, mapping$"Transformation_name" == "Measurement_bigNumber")
listcolnames_Measurement_nonBinary <- selected_mapping_Measurement_nonBinary$Dataset_column

missing_cols <- setdiff(c("Pat", listcolnames_Measurement_nonBinary), colnames(input_data))

if (length(missing_cols) == 0) {
  
  # Select specific columns from input_data based on listcolnames_Measurement_nonBinary
  selected_columns <- input_data[, c("Pat", listcolnames_Measurement_nonBinary), drop = FALSE]
  
  # Use tidyr::pivot_longer to transform to a vertical table
  vertical_table_Measurement_nonBinary <- tidyr::pivot_longer(
    selected_columns,
    cols = -Pat,
    names_to = "Variable",
    values_to = "Value"
  )
  
  # Print the resulting vertical table
  print(vertical_table_Measurement_nonBinary)
  
} else {
  cat("Error: The following specified columns do not exist in input_data:", missing_cols, "\n")
}

# adding the UNITs as well to the dataset
vertical_table_Measurement_nonBinary <- vertical_table_Measurement_nonBinary %>%
  mutate(
    unit = case_when(
      Variable == "WBC" ~ "Gpt/l",
      Variable == "PLT" ~ "Gpt/l",
      Variable == "LDH" ~ "U/l",
      TRUE ~ NA_character_
    )
  )


# adding concept ids for UNITs
vertical_table_Measurement_nonBinary <- vertical_table_Measurement_nonBinary %>%
  mutate(
    unit_concept_id = case_when(
      unit == "Gpt/l" ~ "35949045",
      unit == "Gpt/l" ~ "35949045",
      unit == "U/l" ~ "8645",
      TRUE ~ NA_character_
    )
  )

# get the concept ids for the concepts 
vertical_table_Measurement_nonBinary$measurement_concept_id <- NA

for (i in seq(nrow(vertical_table_Measurement_nonBinary))) {
  current_variable <- vertical_table_Measurement_nonBinary$Variable[i]
  matching_concept_id <- mapping$Concept_id[mapping$Dataset_column == current_variable]
  vertical_table_Measurement_nonBinary$measurement_concept_id[i] <- ifelse(length(matching_concept_id) > 0, matching_concept_id[1], NA)
}


# Check for NAs in specific columns (e.g., Concept_id, Variable, Value)
columns_to_check <- c("measurement_concept_id", "Variable", "Value", "unit", "unit_concept_id")

for (col in columns_to_check) {
  if (any(is.na(vertical_table_Measurement_nonBinary[[col]]))) {
    cat("There are NAs in column", col, "of the vertical_table_Measurement_nonBinary\n")
  } else {
    cat("No NAs found in column", col, "of the vertical_table_Measurement_nonBinary\n")
  }
}

# remove NA entries in the value colum

vertical_table_Measurement_nonBinary <- na.omit(vertical_table_Measurement_nonBinary, cols = c("Value"))

# Print the resulting vertical table
print(vertical_table_Measurement_nonBinary)

# query the patient ids from person table
get_person_id <- function(con, patient_id) {
  query <- glue::glue("
    SELECT person_id FROM cds_cdm_02.person WHERE person_source_value = '{patient_id}';
  ")
  result <- dbGetQuery(con, as.character(query))
  
  if (nrow(result) == 0) {
    warning(paste("No matching person_id found for patient:", patient_id))
    return(NA)
  }
  
  return(result$person_id)
}

Yes_concept_id = 4188539
No_concept_id = 4188540


for (i in 1:nrow(vertical_table_Measurement_nonBinary)) {
  
  person_id <- get_person_id(con, vertical_table_Measurement_nonBinary$Pat[i])
  measurement_concept_id <- vertical_table_Measurement_nonBinary$measurement_concept_id[i]
  measurement_date <- "01.01.1800"
  measurement_type_concept_id <- "32817"
  measurement_source_value <- vertical_table_Measurement_nonBinary$Variable[i]
  value_source_value <- vertical_table_Measurement_nonBinary$Value[i]
  unit_source_value <- vertical_table_Measurement_nonBinary$unit[i]
  unit_concept_id <- vertical_table_Measurement_nonBinary$unit_concept_id[i]
  
  query <- glue::glue("
    INSERT INTO cds_cdm_02.measurement (
      person_id, measurement_concept_id, measurement_date, 
      measurement_type_concept_id, measurement_source_value,
      value_source_value, unit_source_value, unit_concept_id
    )
    VALUES (
      {person_id}, {measurement_concept_id}, '{measurement_date}',
      '{measurement_type_concept_id}','{measurement_source_value}', 
      {value_source_value}, '{unit_source_value}', '{unit_concept_id}'
    );
  ")
  dbExecute(con, as.character(query))
}

dbDisconnect(con)
cat("Measurement data inserted into cds_cdm_02.measurement table.\n")





# #---------------------------------- ----------------------------------  writing the string 

selected_mapping_string <- subset(mapping, mapping$"Transformation_name" == "measurement_string")
listcolnames_string <- selected_mapping_string$Dataset_column

missing_cols <- setdiff(c("Pat", listcolnames_string), colnames(input_data))

if (length(missing_cols) == 0) {
  
  # Select specific columns from input_data based on listcolnames_string
  selected_columns <- input_data[, c("Pat", listcolnames_string), drop = FALSE]
  
  # Use tidyr::pivot_longer to transform to a vertical table
  vertical_table_Measurement_nonBinary_string <- tidyr::pivot_longer(
    selected_columns,
    cols = -Pat,
    names_to = "Variable",
    values_to = "Value"
  )
  
  # Print the resulting vertical table
  print(vertical_table_Measurement_nonBinary_string)
  
} else {
  cat("Error: The following specified columns do not exist in input_data:", missing_cols, "\n")
}

# get the concept ids for the concepts 
vertical_table_Measurement_nonBinary_string$measurement_concept_id <- NA

for (i in seq(nrow(vertical_table_Measurement_nonBinary_string))) {
  current_variable <- vertical_table_Measurement_nonBinary_string$Variable[i]
  matching_concept_id <- mapping$Concept_id[mapping$Dataset_column == current_variable]
  vertical_table_Measurement_nonBinary_string$measurement_concept_id[i] <- ifelse(length(matching_concept_id) > 0, matching_concept_id[1], NA)
}


# Check for NAs in specific columns (e.g., Concept_id, Variable, Value)
columns_to_check <- c("measurement_concept_id", "Variable", "Value", "unit", "unit_concept_id")

for (col in columns_to_check) {
  if (any(is.na(vertical_table_Measurement_nonBinary_string[[col]]))) {
    cat("There are NAs in column", col, "of the vertical_table_Measurement_nonBinary_string\n")
  } else {
    cat("No NAs found in column", col, "of the vertical_table_Measurement_nonBinary_string\n")
  }
}

# remove NA entries in the value colum

vertical_table_Measurement_nonBinary_string <- na.omit(vertical_table_Measurement_nonBinary_string, cols = c("Value"))

# Print the resulting vertical table
print(vertical_table_Measurement_nonBinary_string)

# query the patient ids from person table
get_person_id <- function(con, patient_id) {
  query <- glue::glue("
    SELECT person_id FROM cds_cdm_02.person WHERE person_source_value = '{patient_id}';
  ")
  result <- dbGetQuery(con, as.character(query))
  
  if (nrow(result) == 0) {
    warning(paste("No matching person_id found for patient:", patient_id))
    return(NA)
  }
  
  return(result$person_id)
}

for (i in 1:nrow(vertical_table_Measurement_nonBinary_string)) {
  
  person_id1 <- get_person_id(con, vertical_table_Measurement_nonBinary_string$Pat[i])
  measurement_concept_id1 <- vertical_table_Measurement_nonBinary_string$measurement_concept_id[i]
  measurement_date <- "01.01.1800"
  measurement_type_concept_id <- "32817"
  measurement_source_value1 <- vertical_table_Measurement_nonBinary_string$Variable[i]
  value_source_value1 <- vertical_table_Measurement_nonBinary_string$Value[i]
  
  query <- glue::glue("
    INSERT INTO cds_cdm_02.measurement (
      person_id, measurement_concept_id, measurement_date,
      measurement_type_concept_id, measurement_source_value,
      value_source_value
    )
    VALUES (
      {person_id1}, {measurement_concept_id1}, '{measurement_date}',
      '{measurement_type_concept_id}','{measurement_source_value1}', 
      '{value_source_value1}'
    );
  ")
  dbExecute(con, as.character(query))
}

dbDisconnect(con)
cat("Measurement data inserted into cds_cdm_02.measurement table.\n")













