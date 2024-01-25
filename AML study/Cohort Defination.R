if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
library("RPostgreSQL")
library("DatabaseConnector")
library("CohortMethod")
library("Capr")
library("FeatureExtraction")
library(DBI)

# sh: rm: command not found
# https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

# --------------------------------------------------------- download JDBC driver
Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = getwd())

# Function to download JDBC driver if not exist
downloadJdbcDriver <- function(dbms, pathToDriver) {
  driverFileName <- paste0("postgresqlV42.2.18.zip") 
  
  if (!file.exists(file.path(pathToDriver, driverFileName))) {
    downloadJdbcDrivers(
      dbms = dbms,
      pathToDriver = pathToDriver,
      method = "auto"
    )
  } else {
    cat("JDBC driver already exists in the specified folder.\n")
  }
}

downloadJdbcDriver(dbms = 'postgresql', pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))

# ---------------------------------------------------------- Database Connection
if (!file.exists("config.R")) {
  stop("Error: 'config.R' not found in the current directory.")
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("config.R")

tryCatch({
  ParallelLogger::logInfo("Connecting to the database...")
  
  db_info <- paste("user=", db_user, 
                   " password=", db_password,
                   " dbname=", db,
                   " host=", host_db,
                   " port=", db_port, 
                   "dbms =", dbms, 
                    sep = ",")
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = paste(host_db, "/", db, sep = ""),
                                                                  user = db_user,
                                                                  password = db_password,
                                                                  port = db_port )
  
  ParallelLogger::logInfo("Connected to the database successfully.")
  
}, error = function(e) {
  ParallelLogger::logError("Error connecting to the database:", conditionMessage(e))
  ParallelLogger::logError("Connection details:", db_info)
})

cdmDatabaseName <- 'ohdsi'
cdmDatabaseSchema <- 'cds_cdm'
cohortDatabaseSchema <- 'cds_cdm'
cohortTable <- 'cohort'
vocabularyDatabaseSchema <- 'cds_cdm'
tempEmulationSchema <- NULL

#cohortTable <- 'target_cohort'
# ------------------------------------------------------------ Cohort Definition Manually


# ------------------------------------------------------------ Data extraction

ovariateSettings <- FeatureExtraction::createCovariateSettings( useDemographicsGender = TRUE,
                                                                useDemographicsAge = TRUE, 
                                                                useConditionEraAnyTimePrior = TRUE,
                                                                useObservationAnyTimePrior = TRUE,
                                                                useDrugEraAnyTimePrior = TRUE,
                                                                useProcedureOccurrenceAnyTimePrior = TRUE,
                                                                useMeasurementRangeGroupAnyTimePrior = TRUE)


# ------------------------------------------------------------ Cohort Definition
#cohrts1: all patients who have an event of omplete remission (4014046)
# concept set defination 

CR1ConceptId <- 4014046
OS2yearsConceptId <- 44804077

AMLCR1_conceptSet = cs(
  descendants(CR1ConceptId), 
  name = "AMLCR1")

con <- connect(dbms = dbms, user = db_user, password = db_password, server = paste(host_db, "/", db, sep = ""))


cse <- Capr::getConceptSetDetails(AMLCR1_conceptSet, con = con,
                                 vocabularyDatabaseSchema = vocabularyDatabaseSchema) 


CMLCR1_Cohort = cohort(
  entry = entry(
    observation(cse),
    observationWindow = continuousObservation(),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

CMLCR1_Cohort_json <-  CMLCR1_Cohort %>%
  toCirce() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()

sql <- CirceR::buildCohortQuery(
  expression = CirceR::cohortExpressionFromJson(CMLCR1_Cohort_json),
  options = CirceR::createGenerateOptions(generateStats = FALSE)
)

cohortsToCreate <- tibble::tibble(
  cohortId = 1,
  cohortName = "AML patients with CR1",
  sql = sql
)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "CohortAMLwithCR1")
CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "cds_cdm",
  cohortTableNames = cohortTable
)

cohortsGenerated <- CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTable,
  cohortDefinitionSet = cohortsToCreate)

# Get the cohort counts
cohortCounts <- CohortGenerator::getCohortCounts(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "cds_cdm",
  cohortTable = cohortTable
)

#cohrts1: all patients who have an event of omplete remission (4014046)
# concept set defination 
AMLCR1_conceptSet = cs(
  descendants(4014046), 
  name = "AMLCR1")

CMLCR1_Cohort = cohort(
  entry = entry(
    conditionOccurrence(AMLCR1_conceptSet),
    observationWindow = continuousObservation(),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

CMLCR1_Cohort_json <-  CMLCR1_Cohort %>%
  toCirce() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character()

sql <- CirceR::buildCohortQuery(
  expression = CirceR::cohortExpressionFromJson(CMLCR1_Cohort_json),
  options = CirceR::createGenerateOptions(generateStats = FALSE)
)

cohortsToCreate <- tibble::tibble(
  cohortId = 1,
  cohortName = "AML patients with CR1",
  sql = sql
)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "CohortAMLwithCR1")
CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "cds_cdm",
  cohortTableNames = cohortTableNames
)

cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                                       cohortDatabaseSchema = cohortDatabaseSchema,
                                                       cohortTableNames = cohortTableNames,
                                                       cohortDefinitionSet = cohortsToCreate)


# Print or log the SQL query
cat("Generated SQL Query:\n", sql, "\n")

# Get the cohort counts
cohortCounts <- CohortGenerator::getCohortCounts(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "cds_cdm",
  cohortTable = cohortTableNames$cohortTable
)

print(cohortCounts)



