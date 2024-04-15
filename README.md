# Pipeline to standardize and analyze AML patient data in a multi-center study - a binary classifcation problem.

## Install ohdsi-omop-v5

In order to harmonize the data first an OMOP instance is necessary. If you do not have access to an OMOP database please use the following link to get an OMOP instance runnung in your machine.

installation: https://gitlab.ukdd.de/pub/ohdsi/techstack

## Use the ETL Routes to write your .CSV file datasets to OMOP CDM in the following order.

 - ETL-person.R
 - ETL-condition-binary.R		
 - ETL-measurement-binary.R	
 - ETL-measurement-nonBinary.R	
 - ETL-observation-binary.R
 - ETL-observation-nonBinary.R

## Cohort definition

Define your cohorts using the cohort defination script.

## Cohort retrieval 

Get the cohort from OMOP CDM to the working directory.

## Patient level prediction

Predict Over all Survival in patients in two years using the PatientLevelPrediction-OS.R and the complete remision using PatientLevelPrediction-CR.R script. 






 
