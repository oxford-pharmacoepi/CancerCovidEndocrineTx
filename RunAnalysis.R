


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

# Run incidence analysis to return participants for characterisations ----

if (table_one_analysis == TRUE) {
info(logger, 'RUNNING INCIDENCE ANALYSIS TO RETURN PARTICIPANTS FOR CHARACTERISATIONS')
source(here("2_Analysis","ReturningParticipants_forCharacterisation.R"))
info(logger, 'INCIDENCE ANALYSIS TO RETURN PARTICIPANTS FOR CHARACTERISATIONS RAN')
} else {
  
  print("Not instantiating table 1 cohorts") }


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


