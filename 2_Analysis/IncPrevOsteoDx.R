# ============================================================================ #
#                          Incidence/Prevalence for                            #
#          Osteoporosis, osteopenia, bone fracture diagnoses                   #
#                    and prescriptions of bisphosphonates and denosumab        #
#                    in Breast and Prostate Cancer Cohorts                     #
#                              Nicola Barclay                                  #
#                                25-01-2023                                    #
# ============================================================================ #

## ================== BREAST CANCER ON AROMATASE INHIBITORS ================= ##

print(paste0("- 3. Incidence and Prevalence of AIs-Related Outcomes in Breast Cancer Cohort"))
info(logger, "- 3. Incidence and Prevalence of AIs-Related Outcomes Breast Cancer Cohort")

## ====== SET THE STRATA DENOMINATOR COHORT BREAST CANCER ON AIS ============ ##

print(paste0("- Getting strata population - breast cancer diagnosis on aromatase inhibitors"))
info(logger, "- Getting strata population - breast cancer diagnosis on aromatase inhibitors")

cdm$denominator_breast_AI <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_endocrine_strata",
  strataCohortId = 2,
  strataCohortName = "strata_cohort_2_breast_AIs",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)


cdm$denominator_breast_AI %>% tally()  # to check numbers in denominator_breast population

dpop <- cdm$denominator_breast_AI %>%
  collect() %>%
  left_join(settings(cdm$denominator_breast_AI))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator_breast_AI)

print(paste0("- Got denominator breast cancer diagnosis on aromatase inhibitors"))
info(logger, "- Got denominator breast cancer diagnosis on aromatase inhibitors")

## =================== CALCULATE POINT PREVALENCE =========================== ##

print(paste0("- Getting point prevalence AI-related outcomes in breast cancer populations"))
info(logger, "- Getting point prevalence AI-related outcomes in breast cancer populations")


point_prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_breast_AI",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = outcome_cohorts_3$cohortId, 
  outcomeCohortName = outcome_cohorts_3$cohortName,
  outcomeLookbackDays = 365,
  interval = c("months", "years"),
  timePoint = "start",
  minCellCount = 5,
  verbose = FALSE
)

point_prev %>%
  glimpse()


print(paste0("- Got point prevalence AI-related outcomes in breast cancer populations"))
info(logger, "- Got point prevalence AI-related outcomes in breast cancer populations")



save(point_prev, file = here("Results", db.name, "3_OsteoDx", "PrevAIOutcomesBreast.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence AI-related outcomes in breast cancer populations"))
info(logger, "- Getting incidence AI-related outcomes in breast cancer populations")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_breast_AI",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = outcome_cohorts_3$cohortId, 
  outcomeCohortName = outcome_cohorts_3$cohortName,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365), 
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = FALSE
)

inc %>%
  glimpse()

save(inc, file = here("Results", db.name, "3_OsteoDx", "IncAIOutcomesBreast.RData"))


print(paste0("- Got incidence: AI-related outcomes in breast cancer populations"))
info(logger, "- Got incidence: AI-related outcomes in breast cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: AI-related outcomes in breast cancer populations"))
info(logger, "- Gathering incidence and prevalence results: AI-related outcomes in breast cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev, inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "3_OsteoDx", "StudyResults_AIOutcomesBreast.RData"))

print(paste0("- Got incidence and prevalence results: AI-related outcomes in breast cancer populations"))
info(logger, "- Got incidence and prevalence results: AI-related outcomes in breast cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: AI-related outcomes in breast cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: AI-related outcomes in breast cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevAIOutcomesBreastResults"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: AI-related outcomes in breast cancer populations"))
info(logger, "- Exported incidence and prevalence results: AI-related outcomes in breast cancer populations")


## ===================== PLOTS FOR denominator_breast_AI POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: AI-related outcomes in breast cancer populations denominator_breast_AI 1"))
info(logger, "- Plotting incidence and prevalence results: AI-related outcomes in breast cancer populations denominator_breast_AI 1")

# POINT PREVALENCE IN YEARS FOR ALL AGE STRATA

point_prev_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_id == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_id == "Denosumab" ~ "Denosumab",
                             outcome_cohort_id == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_id == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Aromatase Inhibitor Related Outcomes in Breast Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_yrs_plot)

# save the plot as pdf
analysis.name <- "OsteoDx"
plotname <- paste0("point_prev_yrs_breast_AIs", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name, "3_OsteoDx",plotname),
    width = 10, height = 8)
print(point_prev_yrs_plot, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS FOR ALL AGE STRATA

point_prev_months_plot <- study_results$prevalence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Aromatase Inhibitor Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot

# save the plot as pdf
plotname <- paste0("point_prev_months_breast_AIs", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 8)
print(point_prev_months_plot, newpage = FALSE)
dev.off()


# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Aromatase Inhibitor Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0("inc_yrs_breast_AIs",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name, "3_OsteoDx",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS FOR ALL AGE STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Aromatase Inhibitor Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months_breast_AIs", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()

print(paste0("- Plots of incidence and prevalence results: AI-related outcomes in breast cancer populations denominator_breast_AI 1 done"))
info(logger, "- Plots of incidence and prevalence results: AI-related outcomes in breast cancer populations denominator_breast_AI 1 done")

# ========= PLOTS STRATIFIED BY AGE  ================================== #

print(paste0("- Plotting incidence and prevalence results: AI-related outcomes in breast cancer populations stratified by age"))
info(logger, "- Plotting incidence and prevalence results: AI-related outcomes in breast cancer populations stratified by age ")

# POINT PREVALENCE IN YEARS STRATIFIED BY AGE 
point_prev_yrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  
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
  ggtitle("Point Prevalence of Aromatase Inhibitor Related Outcomes Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_breast_AIs_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 10)
print(point_prev_yrs_plot_s, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  facet_grid(rows=vars(denominator_age_group))
  ggtitle("Point Prevalence of Aromatase Inhibitor Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_months__breast_AIs_s",db.name, analysis.name,".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 10)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()


# INCIDENCE IN YEARS STRATIFIED BY AGE

inc_yrs_plot_s <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_yrs_plot_s <- 
  ggplot(inc_yrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                             ymin = incidence_100000_pys_95CI_lower,
                             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Aromatase Inhibitor Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot_s

# save the plot as pdf
plotname <- paste0("inc_yrs_breast_AIs_s",db.name, analysis.name,  ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 12)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS STRATIFIED BY AGE

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == NULL) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Aromatase Inhibitor Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_breast_AIs_s", db.name, analysis.name,  ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 12)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


print(paste0("- Plots of incidence and prevalence results: Endocrine Treatments in Breast Cancer stratified by age done"))
info(logger, "- Plots of incidence and prevalence results: Endocrine Treatments in Breast Cancer stratified by age done")

print(paste0("- Analysis of Aromatase Inhibitor Related Outcomes in Breast Cancer done"))
info(logger, "- Analysis of Aromatase Inhibitor Related Outcomes in Breast Cancer done")



## ================== BREAST CANCER ON TAMOXIFEN ================= ##

print(paste0("- 1. Incidence and Prevalence of Tamoxifen-GnRH-Related Outcomes in Breast Cancer Cohort"))
info(logger, "- 1. Incidence and Prevalence of Tamoxifen-GnRH-Related Outcomes Breast Cancer Cohort")

## ====== SET THE STRATA DENOMINATOR COHORT BREAST CANCER ON AIS ============ ##

print(paste0("- Getting strata population - breast cancer diagnosis on tamoxifen-GnRH"))
info(logger, "- Getting strata population - breast cancer diagnosis on tamoxifen-GnRH")

cdm$denominator_breast_TAM <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_endocrine_strata",
  strataCohortId = 1,
  strataCohortName = "strata_cohort_2_breast_tamoxifen_gnrh",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Female"),
  daysPriorHistory = 365,
  verbose = TRUE
)


cdm$denominator_breast_TAM %>% tally()  # to check numbers in denominator_breast population

dpop <- cdm$denominator_breast_TAM %>%
  collect() %>%
  left_join(settings(cdm$denominator_breast_TAM))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator_breast_TAM)

print(paste0("- Got denominator breast cancer diagnosis on tamoxifen with gnrh"))
info(logger, "- Got denominator breast cancer diagnosis on tamoxifen with gnrh")

## =================== CALCULATE POINT PREVALENCE =========================== ##

print(paste0("- Getting point prevalence tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Getting point prevalence tamoxifen with gnrh-related outcomes in breast cancer populations")


point_prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_breast_TAM",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = outcome_cohorts_3$cohortId, 
  outcomeCohortName = outcome_cohorts_3$cohortName,
  outcomeLookbackDays = 365,
  interval = c("months", "years"),
  timePoint = "start",
  minCellCount = 5,
  verbose = FALSE
)

point_prev %>%
  glimpse()



print(paste0("- Got point prevalence tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Got point prevalence tamoxifen with gnrh-related outcomes in breast cancer populations")



save(point_prev, file = here("Results", db.name, "3_OsteoDx", "PrevTAMOutcomesBreast.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Getting incidence tamoxifen with gnrh-related outcomes in breast cancer populations")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_breast_TAM",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = outcome_cohorts_3$cohortId, 
  outcomeCohortName = outcome_cohorts_3$cohortName,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365), 
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = FALSE
)

inc %>%
  glimpse()



save(inc, file = here("Results", db.name, "3_OsteoDx", "IncTAMOutcomesBreast.RData"))


print(paste0("- Got incidence: tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Got incidence: tamoxifen with gnrh-related outcomes in breast cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Gathering incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev, inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "3_OsteoDx", "StudyResults_TAMOutcomesBreast.RData"))

print(paste0("- Got incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Got incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevTAMOutcomesBreastResults"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations"))
info(logger, "- Exported incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations")


## ===================== PLOTS FOR denominator_breast_tamoxifen with gnrh POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: tamoxifen with gnrh-related outcomes in breast cancer populations denominator_breast 1")

# POINT PREVALENCE IN YEARS FOR ALL AGE STRATA

point_prev_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_id == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_id == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_id == "Denosumab" ~ "Denosumab",
                             outcome_cohort_id == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_id == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_yrs_plot)

# save the plot as pdf
plotname <- paste0("point_prev_yrs_breast_TAM", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name, "3_OsteoDx",plotname),
    width = 10, height = 8)
print(point_prev_yrs_plot, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS FOR ALL AGE STRATA

point_prev_months_plot <- study_results$prevalence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot

# save the plot as pdf
plotname <- paste0("point_prev_months_breast_TAM", db.name, analysis.name,  ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 8)
print(point_prev_months_plot, newpage = FALSE)
dev.off()


# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0("inc_yrs_breast_TAM",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS FOR ALL AGE STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months_breast_TAM", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()

print(paste0("- Plots of incidence and prevalence results: Tamoxifen with GnRH-related outcomes in breast cancer populations denominator_breast 1 done"))
info(logger, "- Plots of incidence and prevalence results: Tamoxifen with GnRH-related outcomes in breast cancer populations denominator_breast 1 done")

# ========= PLOTS STRATIFIED BY AGE  ================================== #

print(paste0("- Plotting incidence and prevalence results: Tamoxifen with GnRH-related outcomes in breast cancer populations stratified by age"))
info(logger, "- Plotting incidence and prevalence results: Tamoxifen with GnRH-related outcomes in breast cancer populations stratified by age ")

# POINT PREVALENCE IN YEARS STRATIFIED BY AGE 
point_prev_yrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))

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
  ggtitle("Point Prevalence of Tamoxifen with GnRH-Related Outcomes Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_breast_TAM_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 10)
print(point_prev_yrs_plot_s, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  facet_grid(rows=vars(denominator_age_group))
ggtitle("Point Prevalence of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_months__breast_TAM_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 10)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()


# INCIDENCE IN YEARS STRATIFIED BY AGE

inc_yrs_plot_s <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
as.data.frame()

inc_yrs_plot_s <- 
  ggplot(inc_yrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                             ymin = incidence_100000_pys_95CI_lower,
                             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot_s

# save the plot as pdf
plotname <- paste0("inc_yrs_breast_TAM_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 12)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS STRATIFIED BY AGE

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == NULL) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Tamoxifen with GnRH-Related Outcomes in Breast Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_breast_TAM_s", db.name, analysis.name,".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 12)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


print(paste0("- Plots of incidence and prevalence results: Tamoxifen with GnRH-Treatments in Breast Cancer stratified by age done"))
info(logger, "- Plots of incidence and prevalence results: Tamoxifen with GnRH-Treatments in Breast Cancer stratified by age done")

print(paste0("- Analysis of Tamoxifen with GnRH-Related Outcomes in Breast Cancer done"))
info(logger, "- Analysis of Tamoxifen with GnRH-Related Outcomes in Breast Cancer done")




## =========================== PROSTATE CANCER ================================ ##

print(paste0("- 1. Incidence and Prevalence of Endocrine Treatment-related outcomes in Prostate Cancer Cohort"))
info(logger, "- 1. Incidence and Prevalence of Endocrine Treatments-related outcomes in Prostate Cancer Cohort")

## =================== SET THE STRATA DENOMINATOR COHORTS ==================== ##

print(paste0("- Getting strata population - Endocrine Treatment with prostate cancer"))
info(logger, "- Getting strata population - Endocrine Treatment with prostate cancer")

cdm$denominator_prostate_tx <- generateDenominatorCohortSet(
  cdm = cdm,
  startDate = as.Date("2017-01-01"),
  strataTable = "breast_prostate_strata",
  strataCohortId = 3,
  strataCohortName = "strata_cohort_3_prostate",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Male"),
  daysPriorHistory = 365,
  verbose = TRUE
)


cdm$denominator_prostate_tx %>% tally()  # to check numbers in denominator_prostate population

dpop <- cdm$denominator_prostate_tx %>%
  collect() %>%
  left_join(settings(cdm$denominator_prostate_tx))

dpop %>%
  group_by(cohort_definition_id, age_group, sex) %>%
  tally()

# View attrition table
attrition(cdm$denominator_prostate_tx)

print(paste0("- Got denominator_prostate_tx"))
info(logger, "- Got denominator_prostate_tx")

## =================== CALCULATE POINT PREVALENCE =========================== ##

print(paste0("- Getting point prevalence endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Getting point prevalence endocrine tx-related outcomes in prostate cancer populations")


point_prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_prostate_tx",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = outcome_cohorts_3$cohortId, 
  outcomeCohortName = outcome_cohorts_3$cohortName,
  outcomeLookbackDays = 365,
  interval = c("months", "years"),
  timePoint = "start",
  minCellCount = 5,
  verbose = FALSE
)

point_prev %>%
  glimpse()


print(paste0("- Got point prevalence endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Got point prevalence endocrine tx-related outcomes in prostate cancer populations")



save(point_prev, file = here("Results", db.name, "3_OsteoDx", "PrevENDOTxOutcomesProstate.RData"))

## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Getting incidence endocrine tx-related outcomes in prostate cancer populations")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_prostate_tx",
  outcomeTable = outcome_table_name_3, 
  outcomeCohortId = outcome_cohorts_3$cohortId, 
  outcomeCohortName = outcome_cohorts_3$cohortName,
  interval = c("months", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 365), # might not need this line if my cohort definition only includes first event only
  repeatedEvents = FALSE,
  minCellCount = 5,
  verbose = FALSE
)

inc %>%
  glimpse()


save(inc, file = here("Results", db.name, "3_OsteoDx", "IncENDOTxOutcomesProstate.RData"))


print(paste0("- Got incidence: endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Got incidence: endocrine tx-related outcomes in prostate cancer populations")


## ======== GATHER ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Gathering incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Gathering incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations")

study_results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                  resultList=list(point_prev, inc),
                                                  databaseName = db.name)


# save study results as a separate R.data file
save(study_results, file = here("Results", db.name, "3_OsteoDx", "StudyResults_ENDOTxOutcomesProstate.RData"))

print(paste0("- Got incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Got incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: endocrine tx-related outcomes in prostate cancer populations")

exportIncidencePrevalenceResults(result=study_results, 
                                 zipName=paste0(db.name, "IncPrevENDOTxOutcomesProstateResults"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations"))
info(logger, "- Exported incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations")


## ===================== PLOTS FOR denominator_prostate-tx related outcomes POP == 1 ===================== ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations denominator_prostate 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx-related outcomes in prostate cancer populations denominator_prostate 1")

# POINT PREVALENCE IN YEARS FOR ALL AGE STRATA

point_prev_yrs_plot <- study_results$prevalence_estimates %>% 
  filter(denominator_cohort_id == 1) %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Endocrine Treatment-Related Outcomes in Prostate Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

print(point_prev_yrs_plot)

# save the plot as pdf
plotname <- paste0("point_prev_yrs_ENDO_Prostate", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name ,"3_OsteoDx",plotname),
    width = 10, height = 8)
print(point_prev_yrs_plot, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS FOR ALL AGE STRATA

point_prev_months_plot <- study_results$prevalence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot

# save the plot as pdf
plotname <- paste0("point_prev_months_ENDO_Prostate", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 8)
print(point_prev_months_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

# save the plot as pdf
plotname <- paste0("inc_yrs_ENDO_Prostate",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 8)
print(inc_yrs_plot, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS FOR ALL AGE STRATA 

inc_months_plot <- study_results$incidence_estimates %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

# save the plot as pdf
plotname <- paste0("inc_months_ENDO_Prostate", db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 8)
print(inc_months_plot, newpage = FALSE)
dev.off()

print(paste0("- Plots of incidence and prevalence results: endocrine tx-Related Outcomes in prostate cancer populations denominator_prostate 1 done"))
info(logger, "- Plots of incidence and prevalence results: endocrine tx-Related Outcomes in prostate cancer populations denominator_prostate 1 done")

# ========= PLOTS STRATIFIED BY AGE ================================== #

print(paste0("- Plotting incidence and prevalence results: endocrine tx-Related Outcomes in prostate cancer populations stratified by age"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx-Related Outcomes in prostate cancer populations stratified by age")

# POINT PREVALENCE IN YEARS STRATIFIED BY AGE
point_prev_yrs_plot_s <- study_results$prevalence_estimates %>% 
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  
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
  ggtitle("Point Prevalence of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_yrs_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_yrs_ENDO_Prostate_s",db.name, analysis.name,  ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 10)
print(point_prev_yrs_plot_s, newpage = FALSE)
dev.off()


# POINT PREVALENCE IN MONTHS STRATIFIED BY AGE

point_prev_months_plot_s <- study_results$prevalence_estimates %>%  
  filter(analysis_type == "point") %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
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
  ggtitle("Point Prevalence of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Prevalence") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

point_prev_months_plot_s

# save the plot as pdf
plotname <- paste0("point_prev_months_ENDO_Prostate_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 10, height = 10)
print(point_prev_months_plot_s, newpage = FALSE)
dev.off()


# INCIDENCE IN YEARS STRATIFIED BY AGE 

inc_yrs_plot_s <- study_results$incidence_estimates %>%  # need to amend this bit of code to select the estimates relating to per_prev_yrs
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_yrs_plot_s <- 
  ggplot(inc_yrs_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                             ymin = incidence_100000_pys_95CI_lower,
                             ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot_s

# save the plot as pdf
plotname <- paste0("inc_yrs_ENDO_Prostate_s",db.name, analysis.name, ".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 12)
print(inc_yrs_plot_s, newpage = FALSE)
dev.off()




# INCIDENCE IN MONTHS STRATIFIED BY AGE 

inc_months_plot_s <- study_results$incidence_estimates %>%  
  filter(analysis_interval == "months") %>%
  filter(analysis_outcome_washout == NULL) %>% 
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "BoneFracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis"))
  as.data.frame()

inc_months_plot_s <- 
  ggplot(inc_months_plot_s, aes(x = incidence_start_date, y=incidence_100000_pys,
                                ymin = incidence_100000_pys_95CI_lower,
                                ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 400)) +
  facet_grid(rows=vars(denominator_age_group)) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Prostate Cancer in Months Before and After COVID-19 Lockdown Stratified by Age") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot_s

# save the plot as pdf
plotname <- paste0("inc_months_ENDO_Prostate_s", db.name,  analysis.name,".pdf")

pdf(here("Results", db.name,"3_OsteoDx",plotname),
    width = 12, height = 12)
print(inc_months_plot_s, newpage = FALSE)
dev.off()


print(paste0("- Plots of incidence and prevalence results: Endocrine Treatment-Related Outcomes in Prostate Cancer stratified by age done"))
info(logger, "- Plots of incidence and prevalence results: Endocrine Treatment-Related Outcomes in Prostate Cancer stratified by age done")

print(paste0("- Analysis of all Endocrine Treatment-Related Outcomes in Prostate Cancer done"))
info(logger, "- Analysis of all Endocrine Treatment-Related Outcomes in Prostate Cancer done")