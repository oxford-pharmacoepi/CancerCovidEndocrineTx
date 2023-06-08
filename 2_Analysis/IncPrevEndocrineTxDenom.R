# ============================================================================ #
#                       Incidence of Endocrine Treatments                      #
#                     in Breast and Prostate Cancer Cohorts                    #
#                              Nicola Barclay                                  #
#                                08-06-2023                                    #
# ============================================================================ #

## ============== ENDOCRINE TREATMENTS IN DENOMINATOR POP CANCER ============ ##

print(paste0("- 2. Incidence of Endocrine Treatments in Denominator Cohort"))
info(logger, "- 2. Incidence of Endocrine Treatments in Denominator Cohort")

## =================== SET THE STRATA DENOMINATOR COHORTS =================== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm$denominator <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_strata",
  strataCohortId = 1,
  strataCohortName = "strata_cohort_1_breast",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)


cdm$denominator_breast %>% tally()  # to check numbers in denominator_breast population

dpop <- cdm$denominator_breast %>%
  collect() %>%
  left_join(settings(cdm$denominator_breast))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator_breast)

print(paste0("- Got denominator_breast"))
info(logger, "- Got denominator_breast")


















## =========================== BREAST CANCER ================================ ##

print(paste0("- 2. Incidence of Endocrine Treatments in Breast Cancer Cohort"))
info(logger, "- 2. Incidence of Endocrine Treatments in Breast Cancer Cohort")

## =================== SET THE STRATA DENOMINATOR COHORTS ==================== ##

print(paste0("- Getting strata population - breast cancer"))
info(logger, "- Getting strata population - breast cancer")

cdm$denominator_breast <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_strata",
  strataCohortId = 1,
  strataCohortName = "strata_cohort_1_breast",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)


cdm$denominator_breast %>% tally()  # to check numbers in denominator_breast population

dpop <- cdm$denominator_breast %>%
  collect() %>%
  left_join(settings(cdm$denominator_breast))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator_breast)

print(paste0("- Got denominator_breast"))
info(logger, "- Got denominator_breast")


## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx in breast cancer populations"))
info(logger, "- Getting incidence endocrine tx in breast cancer populations")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_breast",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = outcome_cohorts_2$cohortId, # add a filter here to specify which outcome cohorts to focus on specific to breast cancer
  outcomeCohortName = outcome_cohorts_2$cohortName,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365), 
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = FALSE
)

inc %>%
  glimpse()


save(inc, file = here("Results", db.name, "2_EndocrineTx", "IncTxBreast.RData"))


print(paste0("- Got incidence: endocrine tx in breast cancer populations"))
info(logger, "- Got incidence: endocrine tx in breast cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: endocrine tx in breast cancer populations"))
info(logger, "- Gathering incidence and prevalence results: endocrine tx in breast cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev,  inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "2_EndocrineTx", "StudyResults_TxBreast.RData"))

print(paste0("- Got incidence and prevalence results: endocrine tx in breast cancer populations"))
info(logger, "- Got incidence and prevalence results: endocrine tx in breast cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: endocrine tx in breast cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: endocrine tx in breast cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevTxBreastResults"),
                                 outputFolder=here("Results", db.name, "2_EndocrineTx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in breast cancer populations"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in breast cancer populations")


## ===================== PLOTS FOR denominator_breast POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in breast cancer populations denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in breast cancer populations denominator_breast 1")


# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0("inc_yrs_breast",db.name, analysis.name,".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS FOR ALL AGE STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months_breast", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 12, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()

print(paste0("- Plots of incidence and prevalence results: endocrine tx in breast cancer populations denominator_breast 1 done"))
info(logger, "- Plots of incidence and prevalence results: endocrine tx in breast cancer populations denominator_breast 1 done")

# ========= PLOTS Stratified by Age ================================== #

print(paste0("- Plotting incidence and prevalence results: endocrine tx in breast cancer populations stratified by age"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in breast cancer populations stratified by age")

# POINT PREVALENCE IN YEARS Stratified by Age
point_prev_yrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  
  as.data.frame()

point_prev_yrs_plot_s <- 
  ggplot(point_prev_yrs_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                    ymin = prevalence_95CI_lower,
                                    ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Point Prevalence of Endocrine Treatments in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_breast_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 10)
print(point_prev_yrs_plot_s, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS Stratified by Age

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

point_prev_months_plot_s <- 
  ggplot(point_prev_months_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                       ymin = prevalence_95CI_lower,
                                       ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Point Prevalence of Endocrine Treatments in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_months_breast_s",db.name, analysis.name,  ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 10)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()



# INCIDENCE IN YEARS Stratified by Age

inc_yrs_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_yrs_plot_s <- 
  ggplot(inc_yrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                             ymin = incidence_100000_pys_95CI_lower,
                             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot_s

# save the plot as pdf
plotname <- paste0("inc_yrs_breast_s",db.name, analysis.name,".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 12, height = 12)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS Stratified by Age 

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == NULL) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_breast_s", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 12, height = 12)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


print(paste0("- Plots of incidence and prevalence results: Endocrine Treatments in Breast Cancer Stratified by Age done"))
info(logger, "- Plots of incidence and prevalence results: Endocrine Treatments in Breast Cancer Stratified by Age done")

print(paste0("- Analysis of all Endocrine Treatments in Breast Cancer done"))
info(logger, "- Analysis of all Endocrine Treatments in Breast Cancer done")





## =========================== PROSTATE CANCER ================================ ##

print(paste0("- 1. Incidence and Prevalence of Endocrine Treatments in Prostate Cancer Cohort"))
info(logger, "- 1. Incidence and Prevalence of Endocrine Treatments in Prostate Cancer Cohort")

## =================== SET THE STRATA DENOMINATOR COHORTS ==================== ##

print(paste0("- Getting strata population - prostate cancer"))
info(logger, "- Getting strata population - prostate cancer")

cdm$denominator_prostate <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_strata",
  strataCohortId = 2,
  strataCohortName = "strata_cohort_2_prostate",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Male"),
  daysPriorHistory = 365,
  verbose = TRUE
)


cdm$denominator_prostate %>% tally()  # to check numbers in denominator_prostate population

dpop <- cdm$denominator_prostate %>%
  collect() %>%
  left_join(settings(cdm$denominator_prostate))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator_prostate)

print(paste0("- Got denominator_prostate"))
info(logger, "- Got denominator_prostate")

## =================== CALCULATE POINT PREVALENCE =========================== ##

print(paste0("- Getting point prevalence endocrine tx in prostate cancer populations"))
info(logger, "- Getting point prevalence endocrine tx in prostate cancer populations")


point_prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_prostate",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = outcome_cohorts_2$cohortId, # add a filter here to specify which outcome cohorts to focus on specific to prostate cancer
  outcomeCohortName = outcome_cohorts_2$cohortName,
  outcomeLookbackDays = 365,
  interval = c("months", "years"),
  timePoint = "start",
  minCellCount = 5,
  verbose = FALSE
)

point_prev %>%
  glimpse()



print(paste0("- Got point prevalence endocrine tx in prostate cancer populations"))
info(logger, "- Got point prevalence endocrine tx in prostate cancer populations")


save(point_prev, file = here("Results", db.name, "2_EndocrineTx", "PrevTxProstate.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx in prostate cancer populations"))
info(logger, "- Getting incidence endocrine tx in prostate cancer populations")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_prostate",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = outcome_cohorts_2$cohortId, # add a filter here to specify which outcome cohorts to focus on specific to prostate cancer
  outcomeCohortName = outcome_cohorts_2$cohortName,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365), # might not need this line if my cohort definition only includes first event only
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = FALSE
)

inc %>%
  glimpse()


save(inc, file = here("Results", db.name, "2_EndocrineTx", "IncTxProstate.RData"))


print(paste0("- Got incidence: endocrine tx in prostate cancer populations"))
info(logger, "- Got incidence: endocrine tx in prostate cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: endocrine tx in prostate cancer populations"))
info(logger, "- Gathering incidence and prevalence results: endocrine tx in prostate cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev, inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "2_EndocrineTx", "StudyResults_TxProstate.RData"))

print(paste0("- Got incidence and prevalence results: endocrine tx in prostate cancer populations"))
info(logger, "- Got incidence and prevalence results: endocrine tx in prostate cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: endocrine tx in prostate cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: endocrine tx in prostate cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevTxProstateResults"),
                                 outputFolder=here("Results", db.name, "2_EndocrineTx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in prostate cancer populations"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in prostate cancer populations")


## ===================== PLOTS FOR denominator_prostate POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in prostate cancer populations denominator_prostate 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in prostate cancer populations denominator_prostate 1")

# POINT PREVALENCE IN YEARS FOR ALL AGE STRATA

point_prev_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "BreastCancer" ~ "Breast",
                             outcome_cohort_name == "ColorectalCancer" ~ "Colorectal",
                             outcome_cohort_name == "LungCancer" ~ "Lung",
                             outcome_cohort_name == "ProstateCancer" ~ "Prostate")) %>% 
  as.data.frame()

point_prev_yrs_plot <- 
  ggplot(point_prev_yrs_plot, aes(x = prevalence_start_date, y=prevalence,
                                  ymin = prevalence_95CI_lower,
                                  ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of Endocrine Treatments in Prostate Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_yrs_plot)

# save the plot as pdf
plotname <- paste0("point_prev_yrs_prostate", db.name, analysis.name,".pdf")

pdf(here("Results", db.name ,"2_EndocrineTx",plotname),
    width = 10, height = 8)
print(point_prev_yrs_plot, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS FOR ALL AGE STRATA

point_prev_months_plot <- study_results$prevalence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  as.data.frame()

point_prev_months_plot <- 
  ggplot(point_prev_months_plot, aes(x = prevalence_start_date, y=prevalence,
                                     ymin = prevalence_95CI_lower,
                                     ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  ggtitle("Point Prevalence of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot

# save the plot as pdf
plotname <- paste0("point_prev_months_prostate", db.name,  analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 8)
print(point_prev_months_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0("inc_yrs_prostate",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS FOR ALL AGE STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months_prostate", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 12, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()

print(paste0("- Plots of incidence and prevalence results: endocrine tx in prostate cancer populations denominator_prostate 1 done"))
info(logger, "- Plots of incidence and prevalence results: endocrine tx in prostate cancer populations denominator_prostate 1 done")

# ========= PLOTS Stratified by Age ================================== #

print(paste0("- Plotting incidence and prevalence results: endocrine tx in prostate cancer populations Stratified by Age"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in prostate cancer populations Stratified by Age")

# POINT PREVALENCE IN YEARS Stratified by Age
point_prev_yrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  
  as.data.frame()

point_prev_yrs_plot_s <- 
  ggplot(point_prev_yrs_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                    ymin = prevalence_95CI_lower,
                                    ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Point Prevalence of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_prostate_s",db.name, analysis.name,".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 10)
print(point_prev_yrs_plot_s, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS Stratified by Age

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  as.data.frame()

point_prev_months_plot_s <- 
  ggplot(point_prev_months_plot_s, aes(x = prevalence_start_date, y=prevalence,
                                       ymin = prevalence_95CI_lower,
                                       ymax = prevalence_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA)
  ) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Point Prevalence of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_months_prostate_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 10, height = 10)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN YEARS Stratified by Age

inc_yrs_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  as.data.frame()

inc_yrs_plot_s <- 
  ggplot(inc_yrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                             ymin = incidence_100000_pys_95CI_lower,
                             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot_s

# save the plot as pdf
plotname <- paste0("inc_yrs_prostate_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 12, height = 12)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS Stratified by Age 

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == NULL) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "GnRHAgonsists(noADT)" ~ "GnRH (LHRH) Agonists Monotherapy",
                             outcome_cohort_name == "LHRHAntagonists" ~ "GnRH (LHRH) Antagonists",
                             outcome_cohort_name == "AndrogenDeprivationTherapy(withGnRH)" ~ "Androgen Deprivation Therapy with GnRH")) %>% 
  as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_prostate_s", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"2_EndocrineTx",plotname),
    width = 12, height = 12)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


print(paste0("- Plots of incidence and prevalence results: Endocrine Treatments in Prostate Cancer Stratified by Age done"))
info(logger, "- Plots of incidence and prevalence results: Endocrine Treatments in Prostate Cancer Stratified by Age done")

print(paste0("- Analysis of all Endocrine Treatments in Prostate Cancer done"))
info(logger, "- Analysis of all Endocrine Treatments in Prostate Cancer done")