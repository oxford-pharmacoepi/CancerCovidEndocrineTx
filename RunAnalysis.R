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


# table names----

strata_table_name_1 <- paste0(outcome_table_stem,"_breast_prostate_strata") # this is the breast and prostate cohorts to be used as denominator strata
outcome_table_name_1 <- paste0(outcome_table_stem,"_endocrine_tx_table") # this is the table for the endocrine treatments
strata_table_name_2 <- paste0(outcome_table_stem,"_cancer_endocrine_strata") # this is the table for the breast/prostate cancer diagnosis cohorts who are on endocrine treatments to be used as denominator strata
outcome_table_name_2 <- paste0(outcome_table_stem,"_osteo_dx_table") # this is the table for the endocrine-treatment related outcomes of osteoporosis, osteopenia, bon fracture, bisphosphonates and denosumab
denominator_table_name_1 <- paste0(outcome_table_stem,"_denominator_3_time_periods") # this is the table for the denominator before, during and after lockdown
outcome_table_name_3 <- paste0(outcome_table_stem,"_breastprostate3times") # this is the table for breast and prostate cancer before, during and after lockdown



start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# Run incidence analysis of endocrine treatments in denominator population ----
info(logger, 'RUNNING INCIDENCE ANALYSIS OF ENDOCRINE TREATMENTS IN DENOMINATOR POPULATION')
source(here("2_Analysis","IncPrevEndocrineDenom.R"))
info(logger, 'INCIDENCE ANALYSIS OF ENDOCRINE TREATMENTS IN DENOMINATOR POPULATION RAN')

# Run incidence analysis of endocrine treatments ----
info(logger, 'RUNNING INCIDENCE ANALYSIS OF ENDOCRINE TREATMENTS IN BREAST AND PROSTATE CANCERS')
source(here("2_Analysis","IncPrevEndocrineTxCancer.R"))
info(logger, 'INCIDENCE ANALYSIS OF ENDOCRINE TREATMENTS IN BREAST AND PROSTATE CANCERS RAN')

# Run incidence analysis of endocrine-treatment related outcomes ----
info(logger, 'RUNNING INCIDENCE ANALYSIS OF ENDOCRINE TREATMENT RELATED OUTCOMES')
source(here("2_Analysis","IncPrevOsteoDx.R"))
info(logger, 'INCIDENCE ANALYSIS OF ENDOCRINE TREATMENT RELATED OUTCOMES RAN')

# Run incidence analysis of screening tests as outcomes ----
info(logger, 'RUNNING INCIDENCE ANALYSIS OF SCREENING TESTS OUTCOMES')
source(here("2_Analysis","IncScreening.R"))
info(logger, 'INCIDENCE ANALYSIS OF SCREENING TESTS OUTCOMES RAN')

# add code for combining and exporting results - this bit needs editing to reflect all my output folders

study_results <- list()

info(logger, "WRITING CSV FILES")
lapply(names(study_results), function(x) {
  result <- study_results[[x]]
  utils::write.csv(
    result, file = paste0(output_folder, "/", x, ".csv"), row.names = FALSE
  )
})
info(logger, "ZIPPING RESULTS")
output_folder <- basename(output_folder)
zip(
  zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
  files = list.files(output_folder, full.names = TRUE)
)

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)


