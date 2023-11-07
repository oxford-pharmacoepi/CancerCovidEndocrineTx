# ============================================================================ #
#                         Incidence of Endocrine Treatments                    #
#         in Breast and Prostate Cancer Patients Stratified by Region          #
#                         Pre-COVID, lockdown, and Post-lockdown               #
#                              Nicola Barclay                                  #
#                                06-11-2023                                    #
# ============================================================================ #


print(paste0("- 1. Incidence and Prevalence of Endocrine Tx for Breast and Prostate Cancers"))
info(logger, "- 1. Incidence and Prevalence of Endocrine Tx for Breast and Prostate Cancers")


## ============================= PRE-COVID ================================== ##

## ======= Join the CDM observation period with location information ======== ##

allRegions <- cdm$location %>% pull(location_source_value)
cdm$observation_period_original <- cdm$observation_period
cdm$observation_period <- cdm$observation_period_original %>% 
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  #filter(region == !!allRegions[1]) %>%
  mutate(region_collapsed = case_when(region %in% c("Yorkshire  & The Humber", "East Midlands", 
                                                    "West Midlands", "North East", "North West", "East of England", "London", 
                                                    "South East", "South West") ~ "England",
                                      region == "Northern Ireland" ~ "Northern Ireland",
                                      region == "Scotland" ~ "Scotland",
                                      region == "Wales" ~ "Wales")) %>%
  compute()



## ========================================================================== ##
## ========================================================================== ##

print(paste0("- Getting denominator: denominator_pre_england"))
info(logger, "- Getting denominator: denominator_pre_england")    

# MANUAL CALCULATION ACROSS REGIONS   
# ENGLAND
cdm$observation_period <- cdm$observation_period %>% filter(region_collapsed == "England") %>% compute() 

cdm <-generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_pre_england",
  cohortDateRange = as.Date(c("2017-01-01","2020-03-22")),
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)



cdm$denominator_pre_england %>% glimpse()

cdm$denominator_pre_england %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: denominator_pre_england"))
info(logger, "- Got denominator: denominator_pre_england")



## ================= CALCULATE INCIDENCE - ENGLAND PRE-COVID ======================== ##

print(paste0("- Getting incidence: Endocrine Tx for Breast and Prostate Cancers populations pre-covid_england"))
info(logger, "- Getting incidence: Endocrine Tx for Breast and Prostate Cancers populations pre-covid_england")


inc_pre_eng <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_pre_england",
  outcomeTable = outcome_table_name_1, 
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)




inc_pre_eng %>%
  glimpse()


save(inc_pre_eng, file = here("Results", db.name, "Regions", "inc_pre_eng.RData"))


print(paste0("- Got incidence: Endocrine Tx for Breast and Prostate Cancers  populations pre-england"))
info(logger, "- Got incidence: Endocrine Tx for Breast and Prostate Cancers  populations pre-england")






## ============================= LOCKDOWN ================================== ##

## ======= Compute the denominator population ENGLAND LOCKDOWN  ==================== ##

print(paste0("- Getting denominator: england lockdown"))
info(logger, "- Getting denominator: england lockdown")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_lockdown_england",
  cohortDateRange = as.Date(c("2020-03-23","2020-07-03")),
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator_lockdown_england %>% glimpse()

cdm$denominator_lockdown_england %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: england lockdown"))
info(logger, "- Got denominator: england lockdown")



## ================= CALCULATE INCIDENCE - ENGLAND LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations lockdown"))
info(logger, "- Getting incidence: cancer populations lockdown")


inc_lock_eng <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_lockdown_england",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
   interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_lock_eng %>%
  glimpse()


save(inc_lock_eng, file = here("Results", db.name, "Regions", "inc_lock_eng.RData"))


print(paste0("- Got incidence: england cancer populations lockdown"))
info(logger, "- Got incidence: england cancer populations lockdown")








## ============================= POST-LOCKDOWN ============================== ##

## ======= Compute the denominator population POST-LOCKDOWN  =================== ##

print(paste0("- Getting denominator: general population post-lockdown"))
info(logger, "- Getting denominator: general population post-lockdown")


cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_post_england",
  cohortDateRange = as.Date(c("2020-07-04","2022-01-01")),
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)


cdm$denominator_post_england %>% glimpse()

cdm$denominator_post_england %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: post-lockdown"))
info(logger, "- Got denominator: post-lockdown")



## ================= CALCULATE INCIDENCE - POST-LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations post-lockdown"))
info(logger, "- Getting incidence: cancer populations post-lockdown")


inc_post_eng <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_post_england",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_post_eng %>%
  glimpse()


save(inc_post_eng, file = here("Results", db.name, "Regions", "inc_post_eng.RData"))


print(paste0("- Got incidence: england cancer populations post-lockdown"))
info(logger, "- Got incidence: england cancer populations post-lockdown")




## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: england cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: england cancer populations")


exportIncidencePrevalenceResults(resultList=list("inc_pre_eng" = inc_pre_eng, "inc_lock_eng" = inc_lock_eng, "inc_post_eng" = inc_post_eng), 
                                 zipName=paste0(db.name, "IncEndocrineTxResults_England"),
                                 outputFolder=here("Results", db.name, "Regions")) 

print(paste0("- Exported incidence and prevalence results: england cancer populations"))
info(logger, "- Exported incidence and prevalence results: england cancer populations")



################################################################################

# MANUAL CALCULATION ACROSS REGIONS   
# NORTHERN IRELAND

cdm$observation_period <- cdm$observation_period_original %>% 
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  mutate(region_collapsed = case_when(region %in% c("Yorkshire  & The Humber", "East Midlands", 
                                                    "West Midlands", "North East", "North West", "East of England", "London", 
                                                    "South East", "South West") ~ "England",
                                      region == "Northern Ireland" ~ "Northern Ireland",
                                      region == "Scotland" ~ "Scotland",
                                      region == "Wales" ~ "Wales")) %>%
  compute()

cdm$observation_period <- cdm$observation_period %>% filter(region_collapsed == "Northern Ireland") %>% compute() 

cdm <-generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_pre_NI",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2017-01-01","2020-03-22")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = FALSE
)

cdm$denominator_pre_NI %>% glimpse()

cdm$denominator_pre_NI %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: denominator_pre_NI"))
info(logger, "- Got denominator: denominator_pre_NI")



## ================= CALCULATE INCIDENCE - Northern Ireland PRE-COVID ======================== ##

print(paste0("- Getting incidence: cancer populations pre-covid_NI"))
info(logger, "- Getting incidence: cancer populations pre-covid_NI")


inc_pre_NI <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_pre_NI",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_pre_NI %>%
  glimpse()


save(inc_pre_NI, file = here("Results", db.name, "Regions", "inc_pre_NI.RData"))


print(paste0("- Got incidence: cancer populations pre-NI"))
info(logger, "- Got incidence: cancer populations pre-NI")






## ============================= LOCKDOWN ================================== ##

## ======= Compute the denominator population Northern Ireland LOCKDOWN  ==================== ##

print(paste0("- Getting denominator: Northern Ireland lockdown"))
info(logger, "- Getting denominator: Northern Ireland lockdown")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_lockdown_NI",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2020-03-23","2020-07-03")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator_lockdown_NI %>% glimpse()

cdm$denominator_lockdown_NI %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: Northern Ireland lockdown"))
info(logger, "- Got denominator: Northern Ireland lockdown")



## ================= CALCULATE INCIDENCE - Northern Ireland LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations lockdown"))
info(logger, "- Getting incidence: cancer populations lockdown")


inc_lock_NI <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_lockdown_NI",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_lock_NI %>%
  glimpse()


save(inc_lock_NI, file = here("Results", db.name, "Regions", "inc_lock_NI.RData"))


print(paste0("- Got incidence: NI cancer populations lockdown"))
info(logger, "- Got incidence: NI cancer populations lockdown")








## ============================= POST-LOCKDOWN ============================== ##

## ======= Compute the denominator population POST-LOCKDOWN  =================== ##

print(paste0("- Getting denominator: general population post-lockdown"))
info(logger, "- Getting denominator: general population post-lockdown")


cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_post_NI",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2020-07-04","2022-01-01")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)


cdm$denominator_post_NI %>% glimpse()

cdm$denominator_post_NI %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: post-lockdown"))
info(logger, "- Got denominator: post-lockdown")



## ================= CALCULATE INCIDENCE - POST-LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations post-lockdown"))
info(logger, "- Getting incidence: cancer populations post-lockdown")


inc_post_NI <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_post_NI",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_post_NI %>%
  glimpse()


save(inc_post_NI, file = here("Results", db.name, "Regions", "inc_post_NI.RData"))


print(paste0("- Got incidence: NI cancer populations post-lockdown"))
info(logger, "- Got incidence: NI cancer populations post-lockdown")




## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: NI cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: NI cancer populations")


exportIncidencePrevalenceResults(resultList=list("inc_pre_NI" = inc_pre_NI, "inc_lock_NI" = inc_lock_NI, "inc_post_NI" = inc_post_NI), 
                                 zipName=paste0(db.name, "IncEndocrineTxResults_NI"),
                                 outputFolder=here("Results", db.name, "Regions")) 

print(paste0("- Exported incidence and prevalence results: NI cancer populations"))
info(logger, "- Exported incidence and prevalence results: NI cancer populations")





################################################################################

# MANUAL CALCULATION ACROSS REGIONS   
# SCOTLAND

cdm$observation_period <- cdm$observation_period_original %>% 
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  mutate(region_collapsed = case_when(region %in% c("Yorkshire  & The Humber", "East Midlands", 
                                                    "West Midlands", "North East", "North West", "East of England", "London", 
                                                    "South East", "South West") ~ "England",
                                      region == "Northern Ireland" ~ "Northern Ireland",
                                      region == "Scotland" ~ "Scotland",
                                      region == "Wales" ~ "Wales")) %>%
  compute()

cdm$observation_period <- cdm$observation_period %>% filter(region_collapsed == "Scotland") %>% compute() 

cdm <-generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_pre_scotland",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2017-01-01","2020-03-22")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator_pre_scotland %>% glimpse()

cdm$denominator_pre_scotland %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: denominator_pre_scotland"))
info(logger, "- Got denominator: denominator_pre_scotland")



## ================= CALCULATE INCIDENCE - scotland PRE-COVID ======================== ##

print(paste0("- Getting incidence: cancer populations pre-covid_scotland"))
info(logger, "- Getting incidence: cancer populations pre-covid_scotland")


inc_pre_scot <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_pre_scotland",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_pre_scot %>%
  glimpse()


save(inc_pre_scot, file = here("Results", db.name, "Regions", "inc_pre_scot.RData"))


print(paste0("- Got incidence: cancer populations pre-scotland"))
info(logger, "- Got incidence: cancer populations pre-scotland")






## ============================= LOCKDOWN ================================== ##

## ======= Compute the denominator population scotland LOCKDOWN  ==================== ##

print(paste0("- Getting denominator: scotland lockdown"))
info(logger, "- Getting denominator: scotland lockdown")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_lockdown_scotland",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2020-03-23","2020-07-03")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator_lockdown_scotland %>% glimpse()

cdm$denominator_lockdown_scotland %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: scotland lockdown"))
info(logger, "- Got denominator: scotland lockdown")



## ================= CALCULATE INCIDENCE - scotland LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations lockdown"))
info(logger, "- Getting incidence: cancer populations lockdown")


inc_lock_scot <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_lockdown_scotland",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_lock_scot %>%
  glimpse()


save(inc_lock_scot, file = here("Results", db.name, "Regions", "inc_lock_scot.RData"))


print(paste0("- Got incidence: scotland cancer populations lockdown"))
info(logger, "- Got incidence: scotland cancer populations lockdown")




## ============================= POST-LOCKDOWN ============================== ##

## ======= Compute the denominator population POST-LOCKDOWN  =================== ##

print(paste0("- Getting denominator: general population post-lockdown"))
info(logger, "- Getting denominator: general population post-lockdown")


cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_post_scotland",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2020-07-04","2022-01-01")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)


cdm$denominator_post_scotland %>% glimpse()

cdm$denominator_post_scotland %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: post-lockdown"))
info(logger, "- Got denominator: post-lockdown")



## ================= CALCULATE INCIDENCE - POST-LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations post-lockdown"))
info(logger, "- Getting incidence: cancer populations post-lockdown")


inc_post_scot <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_post_scotland",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_post_scot %>%
  glimpse()


save(inc_post_scot, file = here("Results", db.name, "Regions", "inc_post_scot.RData"))


print(paste0("- Got incidence: scotland cancer populations post-lockdown"))
info(logger, "- Got incidence: scotland cancer populations post-lockdown")




## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: scotland cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: scotland cancer populations")


exportIncidencePrevalenceResults(resultList=list("inc_pre_scot" = inc_pre_scot, "inc_lock_scot" = inc_lock_scot, "inc_post_scot" = inc_post_scot), 
                                 zipName=paste0(db.name, "IncCancerResults_scotland"),
                                 outputFolder=here("Results", db.name, "Regions")) 

print(paste0("- Exported incidence and prevalence results: scotland cancer populations"))
info(logger, "- Exported incidence and prevalence results: scotland cancer populations")






################################################################################

# MANUAL CALCULATION ACROSS REGIONS   
# WALES

cdm$observation_period <- cdm$observation_period_original %>% 
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  mutate(region_collapsed = case_when(region %in% c("Yorkshire  & The Humber", "East Midlands", 
                                                    "West Midlands", "North East", "North West", "East of England", "London", 
                                                    "South East", "South West") ~ "England",
                                      region == "Northern Ireland" ~ "Northern Ireland",
                                      region == "Scotland" ~ "Scotland",
                                      region == "Wales" ~ "Wales")) %>%
  compute()

cdm$observation_period <- cdm$observation_period %>% filter(region_collapsed == "Wales") %>% compute() 

cdm <-generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_pre_wales",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2017-01-01","2020-03-22")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator_pre_wales %>% glimpse()

cdm$denominator_pre_wales %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: denominator_pre_wales"))
info(logger, "- Got denominator: denominator_pre_wales")



## ================= CALCULATE INCIDENCE - wales PRE-COVID ======================== ##

print(paste0("- Getting incidence: cancer populations pre-covid_wales"))
info(logger, "- Getting incidence: cancer populations pre-covid_wales")


inc_pre_wales <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_pre_wales",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_pre_wales %>%
  glimpse()


save(inc_pre_wales, file = here("Results", db.name, "Regions", "inc_pre_wales.RData"))


print(paste0("- Got incidence: cancer populations pre-wales"))
info(logger, "- Got incidence: cancer populations pre-wales")






## ============================= LOCKDOWN ================================== ##

## ======= Compute the denominator population wales LOCKDOWN  ==================== ##

print(paste0("- Getting denominator: wales lockdown"))
info(logger, "- Getting denominator: wales lockdown")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_lockdown_wales",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2020-03-23","2020-07-03")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cdm$denominator_lockdown_wales %>% glimpse()

cdm$denominator_lockdown_wales %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: wales lockdown"))
info(logger, "- Got denominator: wales lockdown")



## ================= CALCULATE INCIDENCE - wales LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations lockdown"))
info(logger, "- Getting incidence: cancer populations lockdown")


inc_lock_wales <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_lockdown_wales",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_lock_wales %>%
  glimpse()


save(inc_lock_wales, file = here("Results", db.name, "Regions", "inc_lock_wales.RData"))


print(paste0("- Got incidence: wales cancer populations lockdown"))
info(logger, "- Got incidence: wales cancer populations lockdown")








## ============================= POST-LOCKDOWN ============================== ##

## ======= Compute the denominator population POST-LOCKDOWN  =================== ##

print(paste0("- Getting denominator: general population post-lockdown"))
info(logger, "- Getting denominator: general population post-lockdown")


cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_post_wales",
  strataTable = strata_table_name_1,
  strataCohortId = 1,
  cohortDateRange = as.Date(c("2020-07-04","2022-01-01")),
  ageGroup = list(c(0,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 365,
  temporary = TRUE
)


cdm$denominator_post_wales %>% glimpse()

cdm$denominator_post_wales %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominator: post-lockdown"))
info(logger, "- Got denominator: post-lockdown")



## ================= CALCULATE INCIDENCE - POST-LOCKDOWN ======================== ##

print(paste0("- Getting incidence: cancer populations post-lockdown"))
info(logger, "- Getting incidence: cancer populations post-lockdown")


inc_post_wales <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_post_wales",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE
)

inc_post_wales %>%
  glimpse()


save(inc_post_wales, file = here("Results", db.name, "Regions", "inc_post_wales.RData"))


print(paste0("- Got incidence: wales cancer populations post-lockdown"))
info(logger, "- Got incidence: wales cancer populations post-lockdown")




## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: wales cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: wales cancer populations")


exportIncidencePrevalenceResults(resultList=list("inc_pre_wales" = inc_pre_wales, "inc_lock_wales" = inc_lock_wales, "inc_post_wales" = inc_post_wales), 
                                 zipName=paste0(db.name, "IncEndocrineTxResults_wales"),
                                 outputFolder=here("Results", db.name, "Regions")) 

print(paste0("- Exported incidence and prevalence results: wales cancer populations"))
info(logger, "- Exported incidence and prevalence results: wales cancer populations")





## ==================== PLOTS =============================================== ##


library(ggpubr)

#ADD COLUMN OF REGION AND LOCKDOWN PERIOD
inc_pre_eng['Region']='England'
inc_lock_eng['Region']='England'
inc_post_eng['Region']='England'
inc_pre_eng['Time']='Pre-lockdown'
inc_lock_eng['Time']='Lockdown'
inc_post_eng['Time']='Post-lockdown'

inc_pre_NI['Region']='Northern Ireland'
inc_lock_NI['Region']='Northern Ireland'
inc_post_NI['Region']='Northern Ireland'
inc_pre_NI['Time']='Pre-lockdown'
inc_lock_NI['Time']='Lockdown'
inc_post_NI['Time']='Post-lockdown'

inc_pre_scot['Region']='Scotland'
inc_lock_scot['Region']='Scotland'
inc_post_scot['Region']='Scotland'
inc_pre_scot['Time']='Pre-lockdown'
inc_lock_scot['Time']='Lockdown'
inc_post_scot['Time']='Post-lockdown'

inc_pre_wales['Region']='Wales'
inc_lock_wales['Region']='Wales'
inc_post_wales['Region']='Wales'
inc_pre_wales['Time']='Pre-lockdown'
inc_lock_wales['Time']='Lockdown'
inc_post_wales['Time']='Post-lockdown'

# BIND ALL THE DATAFRAMES TOGETHER

IR_cancers_by_region <- rbind(inc_pre_eng, inc_lock_eng, inc_post_eng, inc_pre_NI, inc_lock_NI, inc_post_NI, inc_pre_scot, inc_lock_scot, inc_post_scot,
                              inc_pre_wales, inc_lock_wales, inc_post_wales)

# RENAME THE OUTCOMES


IR_cancers_by_region <- IR_cancers_by_region %>% mutate(outcome = case_when(outcome_cohort_name == "cohort 1" ~ "Breast",
                                                                            outcome_cohort_name == "cohort 2" ~ "Colorectal",
                                                                            outcome_cohort_name == "cohort 3" ~ "Lung",
                                                                            outcome_cohort_name == "cohort 4" ~ "Prostate" ))

# PLOT THE OVERALL INCIDENCE RATES FOR EACH CANCER SEPARATELY (so that can filter out females for prostate and males for breast) 
# WITH REGIONS ON DIFFERENT LINES  

# Breast
IR_cancers_by_region <- IR_cancers_by_region  %>%
  mutate(`Time` = factor(`Time`, levels=c("Pre-lockdown", "Lockdown","Post-lockdown"))) 

inc_breast_regions <- IR_cancers_by_region %>%
  filter(denominator_cohort_id == 3) %>%
  filter(outcome == "Breast") %>%
  ggplot(aes(x = Time, y=incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=Region, group=Region)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  #facet_wrap(~outcome, nrow=2, scales = "free_y") +
  #ggtitle("") +
  labs(colour = "Region", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.margin=grid::unit(c(1,1,0,1), "cm"))

inc_breast_regions


# Save the plot as jpg
ggsave(here("Results", db.name , "3_Regions", "inc_breast_regions.jpg"), inc_breast_regions, dpi=900, scale = 1, width = 12, height = 9)




# Colorectal
inc_colorectal_regions <- IR_cancers_by_region %>%
  filter(denominator_cohort_id == 1) %>%
  filter(outcome == "Colorectal") %>%
  ggplot(aes(x = Time, y=incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=Region, group=Region)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(colour = "Region", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.margin=grid::unit(c(1,1,0,1), "cm"))

inc_colorectal_regions


# Save the plot as jpg
ggsave(here("Results", db.name , "3_Regions", "inc_colorectal_regions.jpg"), inc_colorectal_regions, dpi=900, scale = 1, width = 12, height = 9)




# Lung
inc_lung_regions <- IR_cancers_by_region %>%
  filter(denominator_cohort_id == 1) %>%
  filter(outcome == "Lung") %>%
  ggplot(aes(x = Time, y=incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=Region, group=Region)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(colour = "Region", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.margin=grid::unit(c(1,1,0,1), "cm"))

inc_lung_regions


# Save the plot as jpg
ggsave(here("Results", db.name , "3_Regions", "inc_lung_regions.jpg"), inc_lung_regions, dpi=900, scale = 1, width = 12, height = 9)




# Prostate
inc_prostate_regions <- IR_cancers_by_region %>%
  filter(denominator_cohort_id == 2) %>%
  filter(outcome == "Prostate") %>%
  ggplot(aes(x = Time, y=incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=Region, group=Region)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(colour = "Region", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.margin=grid::unit(c(1,1,0,1), "cm"))

inc_prostate_regions

# Save the plot as jpg
ggsave(here("Results", db.name , "3_Regions", "inc_prostate_regions.jpg"), inc_prostate_regions, dpi=900, scale = 1, width = 12, height = 9)


# JOIN THE FIGURES

cancer_IR_overall_allRegions<-ggarrange(inc_breast_regions, inc_colorectal_regions, inc_lung_regions, inc_prostate_regions, 
                                        align="hv", ncol=2, nrow=2,
                                        labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),
                                        font.label = list(size = 10),
                                        hjust = c(-0.25,-0.25),
                                        #vjust = -1.1,
                                        common.legend=TRUE, legend="right" )


cancer_IR_overall_allRegions


# Save the plot as jpg
ggsave(here("Results", db.name , "3_Regions", "cancer_IR_overall_allRegions.jpeg"), cancer_IR_overall_allRegions, dpi=900, scale = 1, width = 15, height = 9)

