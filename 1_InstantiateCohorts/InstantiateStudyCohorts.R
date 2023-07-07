# ============================================================================ #
#            INSTANTIATE COHORTS FOR CANCER/COVID ENDOCRINE TX STUDY           #
#                                Nicola Barclay                                #
#                                 08-06-2023                                   #
# ============================================================================ #


# ============================================================================ #
#                    1.  CANCER DIAGNOSES AS DENOMINATOR STRATA                #
# ============================================================================ #
info(logger, "- getting cancer strata")

strata_cohorts_1 <- readCohortSet(here("1_InstantiateCohorts", "CancerStrataCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = strata_cohorts_1,
                         name = strata_table_name_1,
                         overwrite = TRUE) 

#cdm[[strata_table_name_1]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

cohortCount(cdm[[strata_table_name_1]]) %>%  glimpse()

cohortAttrition(cdm[[strata_table_name_1]]) %>%  glimpse()

cohortSet(cdm[[strata_table_name_1]]) %>%  glimpse()


info(logger, "- got cancer strata")

# ============================================================================ #
#                    2.  ENDOCRINE TREATMENTS AS OUTCOMES                      #
# ============================================================================ #


info(logger, "- getting endocrine outcomes")

outcome_cohorts_1 <- readCohortSet(here("1_InstantiateCohorts", "EndocrineTxOutcomeCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_1,
                         name = outcome_table_name_1,
                         overwrite = TRUE) 

#cdm[[outcome_table_name_1]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

cohortCount(cdm[[outcome_table_name_1]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_1]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_1]]) %>%  glimpse()

info(logger, "- got endocrine outcomes")

# ============================================================================ #
#       3.  CANCER DIAGNOSES WITH ENDOCRINE TX AS DENOMINATOR STRATA           #
# ============================================================================ #

info(logger, "- getting cancer and endocrine treatment strata")

strata_cohorts_2 <- readCohortSet(here("1_InstantiateCohorts", "CancerTXStrataCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = strata_cohorts_2,
                         name = strata_table_name_2,
                         overwrite = TRUE) 

#cdm[[strata_table_name_2]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

cohortCount(cdm[[strata_table_name_2]]) %>%  glimpse()

cohortAttrition(cdm[[strata_table_name_2]]) %>%  glimpse()

cohortSet(cdm[[strata_table_name_2]]) %>%  glimpse()



info(logger, "- getting cancer and endocrine treatment strata")

# ============================================================================ #
#                 4.  ENDOCRINE TREATMENT-RELATED OUTCOMES                     #
# ============================================================================ #

info(logger, "- getting endocrine treatment related outcomes")

outcome_cohorts_2 <- readCohortSet(here("1_InstantiateCohorts", "OsteoDxOutcomeCohorts"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_2,
                         name = outcome_table_name_2,
                         overwrite = TRUE) 

#cdm[[outcome_table_name_2]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

cohortCount(cdm[[outcome_table_name_2]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_2]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_2]]) %>%  glimpse()


info(logger, "- got endocrine treatment related outcomes")




# ============================================================================ #
#           5.  DENOMINATOR BEFORE, DURING AND AFTER LOCKDOWN                  #
# ============================================================================ #
info(logger, "- getting denominator populations before, during and after lockdown")

denominator_cohorts_1 <- readCohortSet(here("1_InstantiateCohorts", "Denominator"))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = denominator_cohorts_1,
                         name = denominator_table_name_1,
                         overwrite = TRUE) 

cohortCount(cdm[[denominator_table_name_1]]) %>%  glimpse()

cohortAttrition(cdm[[denominator_table_name_1]]) %>%  glimpse()

cohortSet(cdm[[denominator_table_name_1]]) %>%  glimpse()



info(logger, "- got denominator populations before, during and after lockdown")



# ============================================================================ #
#      6.  BREAST AND PROSTATE CANCER BEFORE, DURING AND AFTER LOCKDOWN        #
# ============================================================================ #
info(logger, "- getting breast and prostate populations before, during and after lockdown")

outcome_cohorts_3 <- readCohortSet(here("1_InstantiateCohorts", "BreastProstate3TimePeriods"))


cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_3,
                         name = outcome_table_name_3,
                         overwrite = TRUE) 

#cdm[[outcome_table_name_3]] %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

cohortCount(cdm[[outcome_table_name_3]]) %>%  glimpse()

cohortAttrition(cdm[[outcome_table_name_3]]) %>%  glimpse()

cohortSet(cdm[[outcome_table_name_3]]) %>%  glimpse()

info(logger, "- got breast and prostate populations before, during and after lockdown")