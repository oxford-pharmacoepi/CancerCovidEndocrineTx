# once you have the incidence results for each of the outcomces, you will need to 
# add these to the "0_DataPrep" folder and name the .csv file from IncidencePrevalence
# package as follows: 
# 0_DataPrep/incidence_estimates_EndoTx_in_breast.csv
# 0_DataPrep/incidence_estimates_EndoTx_in_prostate.csv
# 0_DataPrep/incidence_estimates_EndoDxOutcomes_in_breastAI.csv
# 0_DataPrep/incidence_estimates_EndoDxOutcomes_in_breastTAM.csv
# 0_DataPrep/incidence_estimates_EndoDxOutcomes_in_prostate.csv


start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"



# data preparation ----
info(logger, 'IRR DATA PREPARATION')
source(here("0_DataPrep","DataPrep_for_IRR_EndoTx_in_breast.R"))
source(here("0_DataPrep","DataPrep_for_IRR_EndoTx_in_prostate.R"))
source(here("0_DataPrep","DataPrep_for_IRR_OsteoDX_in_breastAI.R"))
source(here("0_DataPrep","DataPrep_for_IRR_OsteoDX_in_breastTAM.R"))
source(here("0_DataPrep","DataPrep_for_IRR_OsteoDX_in_prostate.R"))
info(logger, 'DONE IRR DATA PREPARATION')

# IRR ANALYSIS AND FOREST PLOTS ----
info(logger, 'IRR DATA PREPARATION')
source(here("3_IRR","IRR_EndocrineTxBreastProstate.R"))
source(here("3_IRR","IRR_EndocrineTxOutcomes_BreastAIs.R"))
source(here("3_IRR","IRR_EndocrineTxOutcomes_BreastTAM.R"))
source(here("3_IRR","IRR_EndocrineTxOutcomes_ProstateEndo.R"))
info(logger, 'DONE ANALYSIS AND FOREST PLOTS')



# add code for combining and exporting results - this bit needs editing to reflect all my output folders

IRR_results <- list()

info(logger, "WRITING CSV FILES FOR IRR RESULTS")
lapply(names(IRR_results), function(x) {
  result <- IRR_results[[x]]
  utils::write.csv(
    result, file = paste0("IRR", output_folder, "/", x, ".csv"), row.names = FALSE
  )
})
info(logger, "ZIPPING RESULTS")
output_folder <- basename(output_folder)
zip(
  zipfile = file.path(paste0(output_folder, "/Results_IRR_", db_name, ".zip")),
  files = list.files(output_folder, full.names = TRUE)
)

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the IRR results!")
Sys.time()-start
readLines(log_file)


