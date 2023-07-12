# ============================================================================ #
#                       STANDARD OHDSI FEATURE EXTRACTION                      #
#                   OF CANCER STRATA COHORTS - ON ENDORCRINE TX                #
#                                Nicola Barclay                                #
#                                 07-02-2023                                   #
# ============================================================================ #


# The code will run the standardised feature extraction package comparing the 
# cancer cohorts before and after lockdown. In this script I use the cohort 
# definitions that are dissected at the date of lockdown.

install.packages("drat")
drat::addRepo("OHDSI")
install.packages("FeatureExtraction")

library(FeatureExtraction)



# CONNECT TO YOUR DATABASE (THE OHDSI WAY) - assuming you have instantiated your cohorts. 
# Note that your data base may require a different driver. See here for details:
# ??jdbcDrivers


connectionDetails <- DatabaseConnector::downloadJdbcDrivers("postgresql",
                                                            here::here())

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server =server,
                                             user = user,
                                             password = password,
                                             port = port ,
                                             pathToDriver = here())


## 1. BREAST AND CANCER BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of breast and prostate cancers"))
info(logger, "- Feature extraction of breast and prostate cancers")

settings <- createTable1CovariateSettings()

BreastStrata <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = strata_table_name_1,
                                      cohortId = 1,
                                      covariateSettings = settings,
                                      aggregated = TRUE)



ProstateStrata <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      cohortDatabaseSchema = results_database_schema,
                                      cohortTable = strata_table_name_1,
                                      cohortId = 2,
                                      covariateSettings = settings,
                                      aggregated = TRUE)


# Breast Cancer Table 1 
BreastStrataResult <- createTable1(BreastStrata, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1)
print(BreastStrataResult, row.names = TRUE, right = FALSE)


# Prostate Cancer Table 1 
ProstateStrataResult <- createTable1(ProstateStrata, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1)
print(ProstateStrataResult, row.names = TRUE, right = FALSE)

# save RData objects
save(BreastStrataResult, ProstateStrataResult, BreastStrata, ProstateStrata, file = here("Results", db.name, "Feature Extraction", "BreastProstateFeatureExtraction.RData"))


Pretty_breast_table <- flextable(BreastStrataResult) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for breast cancer strata cohort") %>% 
  width(width = 1.4) 

save_as_docx(
  "Breast_features_table" = Pretty_breast_table, path = here("Results", db.name, "Feature Extraction", "Breast_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(BreastStrataResult, here("Results", db.name, "Feature Extraction", "Breast_features_table.csv"), row.names = FALSE)


Pretty_prostate_table <- flextable(ProstateStrataResult) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for prostate cancer strata cohort") %>% 
  width(width = 1.4) 

save_as_docx(
  "Prostate_features_table" = Pretty_breast_table, path = here("Results", db.name, "Feature Extraction", "Breast_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(ProstateStrataResult, here("Results", db.name, "Feature Extraction", "Prostate_features_table.csv"), row.names = FALSE)



## 2. BREAST AND CANCER COHORTS ON AIS, TAM OR ENDOCRINE TX FOR PC
##  BASELINE CHARACTERISTICS FOR TABLE 1 FROM FEATURE EXTRACTION PACKAGE

print(paste0("- Feature extraction of breast cancers"))
info(logger, "- Feature extraction of breast cancers")

settings <- createTable1CovariateSettings()

BreastAIStrata <- getDbCovariateData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTable = strata_table_name_2,
                                   cohortId = 1,
                                   covariateSettings = settings,
                                   aggregated = TRUE)

BreastTAMStrata <- getDbCovariateData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdm_database_schema,
                                     cohortDatabaseSchema = results_database_schema,
                                     cohortTable = strata_table_name_2,
                                     cohortId = 2,
                                     covariateSettings = settings,
                                     aggregated = TRUE)


ProstateENDOStrata <- getDbCovariateData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdm_database_schema,
                                     cohortDatabaseSchema = results_database_schema,
                                     cohortTable = strata_table_name_2 ,
                                     cohortId = 3,
                                     covariateSettings = settings,
                                     aggregated = TRUE)


# Breast Cancer on AIs Table 1 
BreastAIStrataResult <- createTable1(BreastAIStrata, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1)
print(BreastAIStrataResult, row.names = TRUE, right = FALSE)

# Breast Cancer on TAM Table 1 
BreastTAMStrataResult <- createTable1(BreastTAMStrata, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1)
print(BreastAIStrataResult, row.names = TRUE, right = FALSE)

# Prostate Cancer on endocrine treatment Table 1 
ProstateENDOStrataResult <- createTable1(ProstateENDOStrata, output = "one column", showCounts = TRUE, showPercent = TRUE, percentDigits = 1, valueDigits = 1)
print(ProstateStrataResult, row.names = TRUE, right = FALSE)

# save RData objects
save(BreastAIStrataResult, BreastTAMStrataResult, ProstateENDOStrataResult, BreastATStrata, BreastTAMStrata,  ProstateENDOStrata, file = here("Results", db.name, "Feature Extraction", "BreastTXProstateTXFeatureExtraction.RData"))


# TABLES BREAST ON AI
Pretty_breast_AI_table <- flextable(BreastAIStrataResult) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for breast cancer on AIs strata cohort") %>% 
  width(width = 1.4) 

save_as_docx(
  "Breast_AI_features_table" = Pretty_breast_AI_table, path = here("Results", db.name, "Feature Extraction", "Breast_AI_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(BreastAIStrataResult, here("Results", db.name, "Feature Extraction", "Breast_AI_features_table.csv"), row.names = FALSE)



# TABLES BREAST ON TAM
Pretty_breast_TAM_table <- flextable(BreastTAMStrataResult) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for breast cancer on TAM strata cohort") %>% 
  width(width = 1.4) 

save_as_docx(
  "Breast_TAM_features_table" = Pretty_breast_TAM_table, path = here("Results", db.name, "Feature Extraction", "Breast_TAM_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(BreastTAMStrataResult, here("Results", db.name, "Feature Extraction", "Breast_TAM_features_table.csv"), row.names = FALSE)



# TABLES PROSTATE ON ANY ENDOCRINE TX
Pretty_prostate_ENDO_table <- flextable(ProstateENDOStrataResult) %>% theme_vanilla() %>% 
  set_caption(caption = "Feature extraction for prostate cancer on any endocrine treatment strata cohort") %>% 
  width(width = 1.4) 

save_as_docx(
  "Prostate_ENDO_features_table" = Pretty_prostate_ENDO_table, path = here("Results", db.name, "Feature Extraction", "Prostate_ENDO_feature_extraction_table.docx"))

# save the table as a csv file
write.csv(ProstateENDOStrataResult, here("Results", db.name, "Feature Extraction", "Prostate_ENDO_features_table.csv"), row.names = FALSE)



