# Pipeline to standardize and analyze AML patient data in a multi-center study - a binary classifcation problem.

This study utilizes our previously developed pipeline (https://github.com/NajiaAhmadi/colab.-AML-study), automating the Extract, Transform, Load (ETL) processes using the R language to enhance reproducibility.

### DataPreprocess

This script calculates the overall survival of patients over two years.

## Install ohdsi-omop-v5

To standardize data, an OMOP instance is required. If you do not have access to an OMOP database, please use the following link to set up an OMOP instance on your machine:

installation Guide: https://gitlab.ukdd.de/pub/ohdsi/techstack

## Use the following scripts to convert your .CSV datasets to the OMOP Common Data Model (CDM) in this specific order:

 - ETL-person.R
 - ETL-condition-binary.R		
 - ETL-measurement-binary.R	
 - ETL-measurement-nonBinary.R	
 - ETL-observation-binary.R
 - ETL-observation-nonBinary.R

## Cohort definition

Define your cohorts using the provided cohort definition script.

## Cohort retrieval 

Retrieve the defined cohort from the OMOP CDM to your working directory.

## Patient level prediction

Predict two-year overall survival and complete remission in patients using the PatientLevelPrediction-OS.R and PatientLevelPrediction-CR.R scripts respectively.













 
