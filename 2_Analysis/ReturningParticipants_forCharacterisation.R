# ============================================================================ #
#                       Incidence of Endocrine Treatments                      #
#                     in Breast and Prostate Cancer Cohorts                    #
#               Code to return participants for characterisation               #
#                              Nicola Barclay                                  #
#                                08-06-2023                                    #
# ============================================================================ #

## ============== ENDOCRINE TREATMENTS IN BREAST CANCER POPULATION ========== ##

print(paste0("- 2. Incidence of Endocrine Treatments in Cancer Cohort"))
info(logger, "- 2. Incidence of Endocrine Treatments in Cancer Cohort")

## ============== SET THE BREAST CANCER STRATA DENOMINATOR COHORTS ========== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  ageGroup = list(c(0,150)),
  sex = c("Both"),
  daysPriorHistory = 365,
  temporary = FALSE
)

count <-cohortCount(cdm$denominator)  

count2 <- cohortSet(cdm$denominator) 

Breast_strata_counts_forChar_strataTRUE <- count %>% left_join(count2)

write.csv(Breast_strata_counts_forChar, file=here::here("Results", db.name, "2_EndocrineTxCancer", "Breast_strata_counts_forChar.csv"))
save(Breast_strata_counts__forChar, file=here::here("Results", db.name, "2_EndocrineTxCancer", "Breast_strata_counts_forChar.Rdata"))


print(paste0("- Got denominator_breast"))
info(logger, "- Got denominator_breast")



cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  ageGroup = list(c(0,150)),
  sex = c("Both"),
  daysPriorHistory = c(0,365),
  strataRequirementsAtEntry = FALSE,
  temporary = FALSE
)

count <-cohortCount(cdm$denominator)  

count2 <- cohortSet(cdm$denominator) 

Breast_strata_counts_forChar_strataTrue <- count %>% left_join(count2)

## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx in breast cancer populations"))
info(logger, "- Getting incidence endocrine tx in breast cancer populations")


IncTxBreast_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL, # add a filter here to specify which outcome cohorts to focus on specific to breast cancer
  #interval = c("months", "quarters", "years"),
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, 90), 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = FALSE,
  returnParticipants = FALSE
)

IncTxBreast %>%
  glimpse()


save(IncTxBreast_overall, file = here("Results", db.name, "2_EndocrineTxCancer", "IncTxBreast_overall.RData"))
save(IncTxBreast_0, file = here("Results", db.name0, "2_EndocrineTxCancer", "IncTxBreast_0.RData"))
save(IncTxBreast_365, file = here("Results", db.name365, "2_EndocrineTxCancer", "IncTxBreast_365.RData"))

print(paste0("- Got incidence: endocrine tx in breast cancer populations"))
info(logger, "- Got incidence: endocrine tx in breast cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: endocrine tx in breast cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: endocrine tx in breast cancer populations")

exportIncidencePrevalenceResults(resultList=list("incidence" = IncTxBreast), 
                                 zipName=paste0(db.name, "IncTxBreast"),
                                 outputFolder=here("Results", db.name, "2_EndocrineTxCancer")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in breast cancer populations"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in breast cancer populations")

