if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
library("devtools")
library("RPostgreSQL")
library("DatabaseConnector")
library("PatientLevelPrediction")
library("FeatureExtraction")
library("CohortMethod")
library("Capr")
library("DBI")

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

# -------------------------------------------------------------- Data extraction

# OMOP database details 
databaseDetails <- createDatabaseDetails(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = 'cds_cdm',
  cdmDatabaseName = 'ohdsi',
  cdmDatabaseId = "AML-CDMv5",
  tempEmulationSchema = 'cds_cdm',
  cohortDatabaseSchema = 'cds_cdm',
  outcomeDatabaseSchema = 'cds_cdm',
  cohortTable = 'target_cohort',
  outcomeTable = 'target_cohort',
  targetId = 3,
  outcomeIds = 2,
  cdmVersion = 5
)

# retrieves the cohort from the database
plpData <- getPlpData(
  databaseDetails = databaseDetails,
  covariateSettings = FeatureExtraction::createCovariateSettings( 
    useDemographicsGender = T,
    useDemographicsAge = T
    #useConditionGroupEraLongTerm = T,
    ),
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings()
)

# -------------------------------------------------------------- Model Design

modelDesign1 <- PatientLevelPrediction::createModelDesign(
  targetId = 3,
  outcomeId = 1,
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
  populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
    firstExposureOnly = T,
    #washoutPeriod = 180, #the number of days prior to index date 
    removeSubjectsWithPriorOutcome = T, priorOutcomeLookback = 180, 
    requireTimeAtRisk = F#, 
    #riskWindowStart = 1, startAnchor = 'cohort start', 
    #riskWindowEnd = , endAnchor = 'cohort start'
  ),
  ovariateSettings <- FeatureExtraction::createCovariateSettings( 
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE, 
    useConditionEraAnyTimePrior = TRUE,
    useObservationAnyTimePrior = TRUE,
    useDrugEraAnyTimePrior = TRUE,
    useProcedureOccurrenceAnyTimePrior = TRUE,
    useMeasurementRangeGroupAnyTimePrior = TRUE
    ), 
  featureEngineeringSettings = NULL, 
  sampleSettings = NULL,
  preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
  modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
  runCovariateSummary = F
    
)

PatientLevelPrediction::runMultiplePlp(
  databaseDetails = databaseDetails,
  modelDesignList = list(
    modelDesign1
  ),
  saveDirectory = file.path(getwd(), 'test_plp')
)





