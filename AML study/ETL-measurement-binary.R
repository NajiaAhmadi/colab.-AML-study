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
  
  dbExecute(con, "TRUNCATE TABLE cds_cdm_02.measurement;")
  
  ParallelLogger::logInfo("Connected to the database successfully.")
}, error = function(e) {
  ParallelLogger::logError("Error connecting to the database:", conditionMessage(e))
  ParallelLogger::logError("Connection details:", db_info)
})


#---------------------------------- Data and Mappings

input_data <- read.csv("20240116_sal_ohsu_data_OS2.csv")
mapping <- read_excel("20240122_Mappings_sal.xlsx", sheet = "Mappings")

#--------------------------------------------- Vertical binary measurement table

selected_mapping <- subset(mapping, mapping$"Transformation_name" == "Measurement_binary")
listcolnames <- selected_mapping$Dataset_column

# Check if specified columns exist in input_data
missing_cols <- setdiff(c("Pat", listcolnames), colnames(input_data))

if (length(missing_cols) == 0) {
  
  # Select specific columns from input_data based on listcolnames
  selected_columns <- input_data[, c("Pat", listcolnames), drop = FALSE]
  
  # Use pivot_longer to transform to a vertical table
  vertical_table <- tidyr::pivot_longer(
    selected_columns,
    cols = -Pat,
    names_to = "Variable",
    values_to = "Value"
  )
  
  # Print the resulting vertical table
  print(vertical_table)
  
} else {
  cat("Error: The following specified columns do not exist in input_data:", missing_cols, "\n")
}

vertical_table$Concept_id <- NA

for (i in seq(nrow(vertical_table))) {
  current_variable <- vertical_table$Variable[i]
  matching_concept_id <- mapping$Concept_id[mapping$Dataset_column == current_variable]
  vertical_table$Concept_id[i] <- ifelse(length(matching_concept_id) > 0, matching_concept_id[1], NA)
}

print(vertical_table)

# clean up the concept id column from the extra elements created by 
# empty spaces such as "\r\n35944932"

for (i in seq(nrow(vertical_table))) {
  current_concept_id <- vertical_table$Concept_id[i]
  cleaned_concept_id <- gsub("[^0-9]", "", current_concept_id)
  vertical_table$Concept_id[i] <- ifelse(nchar(cleaned_concept_id) > 0, cleaned_concept_id, NA)
}

print(vertical_table)

#-------------------------------- Check for NAs in the entire vertical_table
if (any(is.na(vertical_table))) {
  cat("There are NAs in the vertical_table.\n")
} else {
  cat("No NAs found in the vertical_table.\n")
}

# Check for NAs in specific columns (e.g., Concept_id, Variable, Value)
columns_to_check <- c("Concept_id", "Variable", "Value")

for (col in columns_to_check) {
  if (any(is.na(vertical_table[[col]]))) {
    cat("There are NAs in column", col, "of the vertical_table.\n")
  } else {
    cat("No NAs found in column", col, "of the vertical_table.\n")
  }
}

# delete NAs in the value column
vertical_table <- na.omit(vertical_table, cols = c("Value"))

# Check again for NAs in the updated vertical_table
if (any(is.na(vertical_table))) {
  cat("There are still NAs in the vertical_table.\n")
} else {
  cat("No NAs found in the updated vertical_table.\n")
}
#---------------------------------------------------- OMOP measurement table filling

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

for (i in 1:nrow(vertical_table)) {
  # Extract data from the current row in vertical_table
  person_id <- get_person_id(con, vertical_table$Pat[i])
  measurement_concept_id <- vertical_table$Concept_id[i]
  measurement_date <- "01.01.1800"
  measurement_type_concept_id <- "32817"
  measurement_source_value <- vertical_table$Variable[i]
  value_source_value <- vertical_table$Value[i]
  
  # Use NULL for handling missing or unknown data
  #value_as_concept_id <- ifelse(vertical_table$Value[i] == "0", 
  #                              No_concept_id, 
  #                             ifelse(vertical_table$Value[i] == "1", Yes_concept_id, NULL))
  
  query <- glue::glue("
    INSERT INTO cds_cdm_02.measurement (
      person_id, measurement_concept_id, measurement_date, 
      measurement_type_concept_id, measurement_source_value,
      value_source_value
    )
    VALUES (
      {person_id}, {measurement_concept_id}, '{measurement_date}',
      '{measurement_type_concept_id}','{measurement_source_value}', 
      {value_source_value}
    );
  ")
  dbExecute(con, as.character(query))
}

dbDisconnect(con)
cat("Measurement data for binary elements inserted into cds_cdm_02.measurement table.\n")







