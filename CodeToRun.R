# ADD NECESSARY PACKAGES

library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(dbplyr)
library(here)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "...."

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output.folder <-here("Results")
output.folder1 <-here("Results", db.name)
output.folder2<-here("3_IRR", db.name)
output.folder3<-here("3_IRR", db.name, "EndoTxBreast")
output.folder4<-here("3_IRR", db.name, "EndoTxProstate")
output.folder5<-here("3_IRR", db.name, "OsteoDxBreast")
output.folder6<-here("3_IRR", db.name, "OsteoDxProstate")
output.folder7<-here("Results", db.name, "1_EndocrineTxDenom")
output.folder8<-here("Results", db.name, "2_EndocrineTxCancer")
output.folder9<-here("Results", db.name, "3_OsteoDx")
output.folder10<-here("Results", db.name, "4_Characterisations")

# output files ----
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}

if (!file.exists(output.folder4)){
  dir.create(output.folder4, recursive = TRUE)}

if (!file.exists(output.folder5)){
  dir.create(output.folder5, recursive = TRUE)}

if (!file.exists(output.folder6)){
  dir.create(output.folder6, recursive = TRUE)}

if (!file.exists(output.folder7)){
  dir.create(output.folder7, recursive = TRUE)}

if (!file.exists(output.folder8)){
  dir.create(output.folder8, recursive = TRUE)}

if (!file.exists(output.folder9)){
  dir.create(output.folder9, recursive = TRUE)}

if (!file.exists(output.folder10)){
  dir.create(output.folder10, recursive = TRUE)}


# Database connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
# you may need to install another package for this 
# eg for postgres 
# db <- dbConnect(
#   RPostgres::Postgres(), 
#   dbname = server_dbi, 
#   port = port, 
#   host = host, 
#   user = user,
#   password = password
# )
db <- dbConnect("....")

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "...."

# The name of the schema where results tables will be created 
results_database_schema <- "...."

# Name of stem outcome table in the result schema where the outcome cohorts will
# be stored. 
# Notes: 
# - if there is an existing table in your results schema with the same names it
#   will be overwritten
# - more than one cohort will be created
# - name must be lower case
stem_table <- "...."

strata_table_name_1 <- paste0(outcome_table_stem,"_breast_prostate_strata") # this is the breast and prostate cohorts to be used as denominator strata
outcome_table_name_1 <- paste0(outcome_table_stem,"_endocrine_tx_table") # this is the table for the endocrine treatments
strata_table_name_2 <- paste0(outcome_table_stem,"_cancer_endocrine_strata") # this is the table for the breast/prostate cancer diagnosis cohorts who are on endocrine treatments to be used as denominator strata
outcome_table_name_2 <- paste0(outcome_table_stem,"_osteo_dx_table") # this is the table for the endocrine-treatment related outcomes of osteoporosis, osteopenia, bon fracture, bisphosphonates and denosumab
denominator_table_name_1 <- paste0(outcome_table_stem,"_denom_3_times") # this is the table for the denominator before, during and after lockdown
outcome_table_name_3 <- paste0(outcome_table_stem,"_breast_prostate_3_times") # this is the table for breast and prostate cancer before, during and after lockdown
feature_disease_table_name_1 <-paste0(outcome_table_stem,"_disease_final") # this is the table for the characterisation of diseases - danielle's
feature_medication_table_name_1 <-paste0(outcome_table_stem,"_medication_final") # this is the table for the characterisation of medications - danielle's

# run table 1
table_one_analysis <- TRUE

# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)
# check database connection
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Run the study ------
source(here("RunAnalysis.R"))
# after the study is run you should have a zip folder in your output folder to share

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")

# once you have the incidence results for each of the outcomces, you will need to 
# add these to the "0_DataPrep" folder and name the .csv file from IncidencePrevalence
# package as follows: 
# 0_DataPrep/incidence_estimates_EndoTx_in_breast.csv
# 0_DataPrep/incidence_estimates_EndoTx_in_prostate.csv
# 0_DataPrep/incidence_estimates_EndoDxOutcomes_in_breastAI.csv
# 0_DataPrep/incidence_estimates_EndoDxOutcomes_in_breastTAM.csv
# 0_DataPrep/incidence_estimates_EndoDxOutcomes_in_prostate.csv

# Then you can run the IRR file
source(here("RunStudy_IRR.R"))

# Disconnect from the database
dbDisconnect(db)