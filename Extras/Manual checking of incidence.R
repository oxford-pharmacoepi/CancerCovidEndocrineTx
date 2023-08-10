# Checking monthly incidence rates manually

# Breast cancer strata population, starting from 2017 in the cohort definition
# count how many 'events' of araomatase inhibitors (as a cohort definition) there 
# are per month and compare with what is derived from IncidencePrevalence

# GET THE CDM WITH THE BREAST CANCER STRATA AND ENDOCRINE TREATMENT OUTCOMES
# TABLES ADDED

library(PatientProfiles)
library(DrugUtilisation)

cdm$condition_occurrence %>% glimpse()

# count how many patients in breast and prostate cancer strata cohorts
cdm$nb_cancercovid_endotx_breast_prostate_strata2017 %>% group_by(cohort_definition_id) %>% tally()

cohortCount(cdm[[strata_table_name_1b]]) %>%  glimpse()
cohortSet(cdm[[strata_table_name_1b]]) %>%  glimpse()

# count how many patients in the endocrine treatment cohorts
cdm$nb_cancercovid_endotx_endocrine_tx_table_first %>% group_by(cohort_definition_id) %>% tally()

cohortCount(cdm[[outcome_table_name_1b]]) %>%  glimpse()
cohortSet(cdm[[outcome_table_name_1b]]) %>%  glimpse()


# ADD DAYS PRIOR HISTORY TO STRATA COHORT TABLES
cdm$nb_cancercovid_endotx_breast_prostate_strata2017  <- cdm$nb_cancercovid_endotx_breast_prostate_strata2017  %>%
  addPriorHistory(cdm=cdm)

cdm$nb_cancercovid_endotx_breast_prostate_strata2017 %>% glimpse()


# FILTER ONLY THOSE WITH BREAST CANCER DIAGNOSIS IN JANUARY 2017, WITH 365 PH, AND FOR BREAST CANCER AND SAVE INTO THE COHORT TABLE
cdm$breast_jan17  <- cdm$nb_cancercovid_endotx_breast_prostate_strata2017 %>% filter(cohort_definition_id == 1) %>% 
  filter(cohort_start_date >="2017-01-01" & cohort_start_date <"2017-02-01") %>%
  filter(prior_history >= 365)

cdm$breast_jan17 %>% tally()
cdm$breast_jan17 %>% glimpse()


# ADD DATE OF FIRST EXPOSURE OF AROMATASE INHIBITORS ANYTIME IN HISTORY OR FUTURE FROM COHORT START DATE
cdm$breast_jan17 <- cdm$breast_jan17 %>%
  addCohortIntersectDate(cdm = cdm,
    targetCohortTable = outcome_table_name_1b,
    targetCohortId = c(1,2,8,9),
    order = "first",
    window = c(-Inf, Inf)
  )

cdm$breast_jan17 %>% glimpse()

# filter those that had the outcome within Jan 2017
cdm$breast_jan17 %>% filter(aromataseinhibitors_minf_to_inf >="2017-01-01" & aromataseinhibitors_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$breast_jan17 %>% filter(tamoxifen_minf_to_inf >="2017-01-01" & tamoxifen_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$breast_jan17 %>% filter(tamoxifen_withgnrhagonistsorantagonists_minf_to_inf >="2017-01-01" & tamoxifen_withgnrhagonistsorantagonists_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$breast_jan17 %>% filter(aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf >="2017-01-01" & aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()



################################################################################

# JOINING WITH OBSERVATION PERIOD FOR BREAST CANCER IN JAN 2017

cdm$BreastJan2017 <- cdm$nb_cancercovid_endotx_breast_prostate_strata2017 %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, prior_history) %>%
  filter(cohort_definition_id ==1) %>%
  filter(prior_history >= 365) %>%
  inner_join(cdm$observation_period %>% 
      select("subject_id" = "person_id", observation_period_start_date, observation_period_end_date)) %>%
      compute() # compute saves in a temp table, collect saves in r memory
      
cdm$BreastJan2017 %>% glimpse()

# view breast cancer patients within a specified observation period
cdm$BreastJan2017 <- cdm$BreastJan2017 %>% filter(cohort_start_date <= "2017-01-31") %>% # include only those that had their breast cancer diagnosis prior to end of jan 2017
  filter(observation_period_end_date >="2017-01-01" & observation_period_start_date <="2017-01-31") %>% # include only those taht were observed within this time
  distinct() %>%
  compute()


cdm$BreastJan2017 %>% glimpse()


# ADD OUTCOME COHORT COUNTS - this doesn't work, possibly because i have modified the cohort table to include the observation periods
cdm$BreastJan2017 <- cdm$BreastJan2017 %>%
  addCohortIntersectDate(cdm = cdm,
                         targetCohortTable = outcome_table_name_1b,
                         targetCohortId = c(1,2,8,9),
                         order = "first",
                         window = c(-Inf, Inf)
                )

cdm$BreastJan2017 %>% glimpse()


# COUNT HOW MANY BREAST CANCER PATIENTS THERE ARE WITH A FLAG FOR EACH OF THE OUTCOMES

# filter those that had the outcome within Jan 2017
cdm$BreastJan2017 %>% filter(aromataseinhibitors_minf_to_inf >="2017-01-01" & aromataseinhibitors_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2017 %>% filter(tamoxifen_minf_to_inf >="2017-01-01" & tamoxifen_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2017 %>% filter(tamoxifen_withgnrhagonistsorantagonists_minf_to_inf >="2017-01-01" & tamoxifen_withgnrhagonistsorantagonists_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2017 %>% filter(aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf >="2017-01-01" & aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()




################################################################################

# JOINING WITH OBSERVATION PERIOD FOR BREAST CANCER IN JAN 2018

cdm$BreastJan2018 <- cdm$nb_cancercovid_endotx_breast_prostate_strata2017 %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, prior_history) %>%
  filter(cohort_definition_id ==1) %>%
  filter(prior_history >= 365) %>%
  inner_join(cdm$observation_period %>% 
               select("subject_id" = "person_id", observation_period_start_date, observation_period_end_date)) %>%
  compute() # compute saves in a temp table, collect saves in r memory

cdm$BreastJan2018 %>% glimpse()

# view breast cancer patients within a specified observation period
cdm$BreastJan2018 <- cdm$BreastJan2018 %>% filter(cohort_start_date <= "2018-01-31") %>% # include only those that had their breast cancer diagnosis prior to end of jan 2017
  filter(observation_period_end_date >="2018-01-01" & observation_period_start_date <="2018-01-31") %>% # include only those taht were observed within this time
  distinct() %>%
  compute()


cdm$BreastJan2018 %>% glimpse()


# ADD OUTCOME COHORT COUNTS - 
cdm$BreastJan2018 <- cdm$BreastJan2018 %>%
  addCohortIntersectDate(cdm = cdm,
                         targetCohortTable = outcome_table_name_1b,
                         targetCohortId = c(1,2,8,9),
                         order = "first",
                         window = c(-Inf, Inf)
  )

cdm$BreastJan2018 %>% glimpse()


# COUNT HOW MANY BREAST CANCER PATIENTS THERE ARE WITH A FLAG FOR EACH OF THE OUTCOMES

# filter those that had the outcome within Jan 2018
cdm$BreastJan2018 %>% filter(aromataseinhibitors_minf_to_inf >="2018-01-01" & aromataseinhibitors_minf_to_inf <="2018-01-31") %>% # include only those that had the outcome in jan 2018  distinct() %>%
  tally()

cdm$BreastJan2018 %>% filter(tamoxifen_minf_to_inf >="2018-01-01" & tamoxifen_minf_to_inf <="2018-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2018 %>% filter(tamoxifen_withgnrhagonistsorantagonists_minf_to_inf >="2018-01-01" & tamoxifen_withgnrhagonistsorantagonists_minf_to_inf <="2018-01-31") %>% # include only those that had the outcome in jan 2018  distinct() %>%
  tally()

cdm$BreastJan2018 %>% filter(aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf >="2018-01-01" & aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf <="2018-01-31") %>% # include only those that had the outcome in jan 2018  distinct() %>%
  tally()

# TOTAL N PERSONS IN BREAST IN JAN 2018
cdm$BreastJan2018 %>% tally()






################################################################################

# JOINING WITH OBSERVATION PERIOD FOR BREAST CANCER IN JAN 2017

cdm$BreastJan2017 <- cdm$nb_cancercovid_endotx_breast_prostate_strata2017 %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, prior_history) %>%
  filter(cohort_definition_id ==1) %>%
  filter(prior_history >= 365) %>%
  inner_join(cdm$observation_period %>% 
               select("subject_id" = "person_id", observation_period_start_date, observation_period_end_date)) %>%
  compute() # compute saves in a temp table, collect saves in r memory

cdm$BreastJan2017 %>% glimpse()

# view breast cancer patients within a specified observation period
cdm$BreastJan2017 <- cdm$BreastJan2017 %>% filter(cohort_start_date <= "2017-01-31") %>% # include only those that had their breast cancer diagnosis prior to end of jan 2017
  filter(observation_period_end_date >="2017-01-01" & observation_period_start_date <="2017-01-31") %>% # include only those taht were observed within this time
  distinct() %>%
  compute()


cdm$BreastJan2017 %>% glimpse()


# ADD OUTCOME COHORT COUNTS - this doesn't work, possibly because i have modified the cohort table to include the observation periods
cdm$BreastJan2017 <- cdm$BreastJan2017 %>%
  addCohortIntersectDate(cdm = cdm,
                         targetCohortTable = outcome_table_name_1b,
                         targetCohortId = c(1,2,8,9),
                         order = "first",
                         window = c(-Inf, Inf)
  )

cdm$BreastJan2017 %>% glimpse()


# COUNT HOW MANY BREAST CANCER PATIENTS THERE ARE WITH A FLAG FOR EACH OF THE OUTCOMES

# filter those that had the outcome within Jan 2017
cdm$BreastJan2017 %>% filter(aromataseinhibitors_minf_to_inf >="2017-01-01" & aromataseinhibitors_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2017 %>% filter(tamoxifen_minf_to_inf >="2017-01-01" & tamoxifen_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2017 %>% filter(tamoxifen_withgnrhagonistsorantagonists_minf_to_inf >="2017-01-01" & tamoxifen_withgnrhagonistsorantagonists_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2017 %>% filter(aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf >="2017-01-01" & aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf <="2017-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()




################################################################################

# JOINING WITH OBSERVATION PERIOD FOR BREAST CANCER IN JAN 2010

# ADD DAYS PRIOR HISTORY TO STRATA COHORT TABLES
cdm$nb_cancercovid_endotx_breast_prostate_strata  <- cdm$nb_cancercovid_endotx_breast_prostate_strata  %>%
  addPriorHistory(cdm=cdm)

cdm$BreastJan2010 <- cdm$nb_cancercovid_endotx_breast_prostate_strata %>%
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date, prior_history) %>%
  filter(cohort_definition_id ==1) %>%
  filter(prior_history >= 365) %>%
  inner_join(cdm$observation_period %>% 
               select("subject_id" = "person_id", observation_period_start_date, observation_period_end_date)) %>%
  compute() # compute saves in a temp table, collect saves in r memory

cdm$BreastJan2010 %>% glimpse()

# view breast cancer patients within a specified observation period
cdm$BreastJan2010 <- cdm$BreastJan2010 %>% filter(cohort_start_date <= "2010-01-31") %>% # include only those that had their breast cancer diagnosis prior to end of jan 2017
  filter(observation_period_end_date >="2010-01-01" & observation_period_start_date <="2010-01-31") %>% # include only those taht were observed within this time
  distinct() %>%
  compute()


cdm$BreastJan2010 %>% glimpse()


# ADD OUTCOME COHORT COUNTS - 
cdm$BreastJan2010 <- cdm$BreastJan2010 %>%
  addCohortIntersectDate(cdm = cdm,
                         targetCohortTable = outcome_table_name_1b,
                         targetCohortId = c(1,2,8,9),
                         order = "first",
                         window = c(-Inf, Inf)
  )

cdm$BreastJan2010 %>% glimpse()


# COUNT HOW MANY BREAST CANCER PATIENTS THERE ARE WITH A FLAG FOR EACH OF THE OUTCOMES

# filter those that had the outcome within Jan 2010
cdm$BreastJan2010 %>% filter(aromataseinhibitors_minf_to_inf >="2010-01-01" & aromataseinhibitors_minf_to_inf <="2010-01-31") %>% # include only those that had the outcome in jan 2018  distinct() %>%
  tally()

cdm$BreastJan2010 %>% filter(tamoxifen_minf_to_inf >="2010-01-01" & tamoxifen_minf_to_inf <="2010-01-31") %>% # include only those that had the outcome in jan 2017  distinct() %>%
  tally()

cdm$BreastJan2010 %>% filter(tamoxifen_withgnrhagonistsorantagonists_minf_to_inf >="2010-01-01" & tamoxifen_withgnrhagonistsorantagonists_minf_to_inf <="2010-01-31") %>% # include only those that had the outcome in jan 2018  distinct() %>%
  tally()

cdm$BreastJan2010 %>% filter(aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf >="2010-01-01" & aromataseinhibitors_withgnrhagonistsorantagonists_minf_to_inf <="2010-01-31") %>% # include only those that had the outcome in jan 2018  distinct() %>%
  tally()

# TOTAL N PERSONS IN BREAST IN JAN 2018
cdm$BreastJan2010 %>% tally()
