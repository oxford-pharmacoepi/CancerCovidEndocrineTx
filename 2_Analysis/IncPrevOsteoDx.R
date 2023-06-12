# ============================================================================ #
#             Incidence of Endocrine Treatment-related outcomes                #
#           in Breast and Prostate Cancer Patients on Endocrine tx             #
#                              Nicola Barclay                                  #
#                                08-06-2023                                    #
# ============================================================================ #

## == ENDOCRINE TREATMENT-RELATED OUTCOMES IN BREAST CANCER POPULATION ====== ##

print(paste0("- 2. Incidence of Endocrine Treatment-Related Outcomes in Breast Cancer Cohort"))
info(logger, "- 2. Incidence of Endocrine Treatment-Related Outcomes in Breast Cancer Cohort")

## ============== SET THE BREAST CANCER AI TREATMENT STRATA DENOMINATOR COHORTS ========== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  strataTable = strata_table_name_2,
  strataCohortId = 1,
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both","Female","Male"),
  daysPriorHistory = 365,
  temporary = TRUE
)

cohortCount(cdm$denominator) # CHECK NAME OF OUTCOME COHORT IN TABLE AS MIGHT NOT LIKE BRACKETS

cohortSet(cdm$denominator) 

print(paste0("- Got denominator_breast_TX"))
info(logger, "- Got denominator_breast_TX")



## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence treatment related outcomes in breast cancer populations on AIs"))
info(logger, "- Getting incidence treatment related outcomes in breast cancer populations on AIs")


IncTxOutcomesBreastAI <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = NULL, # add a filter here to specify which outcome cohorts to focus on specific to breast cancer
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, 90), 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxOutcomesBreastAI %>%
  glimpse()


save(IncTxOutcomesBreastAI, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesBreastAI.RData"))


print(paste0("- Got incidence: treatment related outcomes in breast cancer populations on AIs"))
info(logger, "- Got incidence: treatment related outcomes in breast cancer populations on AIs")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: treatment related outcomes in breast cancer populations on AIs"))
info(logger, "- Exporting incidence and Prevalence results: treatment related outcomes in breast cancer populations on AIs")

exportIncidencePrevalenceResults(resultList=list("incidence" = IncTxOutcomesBreastAI), 
                                 zipName=paste0(db.name, "IncTxOutcomesBreastAI"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in breast cancer populations on AIs"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in breast cancer populations on AIs")


## ============ PLOTS FOR endocrine tx in breast cancer pop  on AIs ================= ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in breast cancer populations  on AIs denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in breast cancer populations  on AIs denominator_breast 1")



# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- IncTxOutcomesBreastAI %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 0) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "Bone Fracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  #scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
    ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Years in Breast Cancer Patients on Aromoatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".jpg")), inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)




# INCIDENCE IN MONTHS FOR ALL AGE STRATA

inc_months_plot <- IncTxOutcomesBreastAI %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 0) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "Bone Fracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  #scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="6 months") +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Months in Breast Cancer Patients on Aromoatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as jpg
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".jpg")), inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)




# INCIDENCE IN QUARTERS FOR ALL AGE STRATA


inc_qrs_plot <- IncTxOutcomesBreastAI %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 0) %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "Bone Fracture" ~ "Bone Fracture",
                             outcome_cohort_name == "Denosumab" ~ "Denosumab",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis")) %>% 
  as.data.frame()

inc_qrs_plot <- 
  ggplot(inc_qrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  #scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months") +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Quarters in Breast Cancer Patients on Aromoatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_qrs_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_qrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".jpg")), inc_qrs_plot, dpi=600, scale = 1, width = 12, height = 9)



print(paste0("- Analysis of all Endocrine Treatment-Related Outcomes in breast cancer patients done"))
info(logger, "- Analysis of all Endocrine Treatment-Related Outcomes in breast cancer patients done")

rm(c(inc_yrs_plot, inc_months_plot, inc_qrs_plot))




# upto here - next run the breast pop on tamoxifen and then prostate cancer on any endocrine tx

## =========================== PROSTATE CANCER ================================ ##

print(paste0("- 1. Incidence and Prevalence of Endocrine Treatments in Prostate Cancer Cohort"))
info(logger, "- 1. Incidence and Prevalence of Endocrine Treatments in Prostate Cancer Cohort")

## ============== SET THE PROSTATE CANCER STRATA DENOMINATOR COHORTS ========== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  strataTable = strata_table_name_1,
  strataCohortId = 2,
  strataCohortName = "ProstateCancerStrata",
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = "Male",
  daysPriorHistory = 365,
  temporary = TRUE
)

cohortCount(cdm$ProstateCancerStrata)

cohortSet(cdm$ProstateCancerStrata) 

print(paste0("- Got denominator_Prostate"))
info(logger, "- Got denominator_Prostate")



## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx in Prostate cancer populations"))
info(logger, "- Getting incidence endocrine tx in Prostate cancer populations")


IncTxProstate <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "ProstateCancerStrata",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = c(3,4,5,6,7),
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 90), 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxProstate %>%
  glimpse()


save(IncTxProstate, file = here("Results", db.name, "3_OsteoDx", "IncTxProstate.RData"))


print(paste0("- Got incidence: endocrine tx in Prostate cancer populations"))
info(logger, "- Got incidence: endocrine tx in Prostate cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: endocrine tx in Prostate cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: endocrine tx in Prostate cancer populations")

exportIncidencePrevalenceResults(resultList=list("incidence" = IncTxProstate), 
                                 zipName=paste0(db.name, "IncTxProstate"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in Prostate cancer populations"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in Prostate cancer populations")


## ============ PLOTS FOR endocrine tx in Prostate cancer pop ================= ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in Prostate cancer populations denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in Prostate cancer populations denominator_breast 1")



# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- IncTxProstate %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Years in Prostate Cancer Patients Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot


# Save the plot as jpg
ggsave(here("Results", db.name , "3_OsteoDx", paste0(EndoTxProstateCancer_inc_yrs_plot, ".jpg")), inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)



# INCIDENCE IN MONTHS FOR ALL AGE STRATA


inc_months_plot <- IncTxProstate %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Months in Prostate Cancer Patients Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_months_plot


# Save the plot as jpg
ggsave(here("Results", db.name , "3_OsteoDx", paste0(EndoTxProstateCancer_inc_months_plot, ".jpg")), inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)



# INCIDENCE IN QUARTERS FOR ALL AGE STRATA

inc_qrs_plot <- IncTxProstate %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == NULL) %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens")) %>% 
  as.data.frame()

inc_qrs_plot <- 
  ggplot(inc_qrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Quarters in Prostate Cancer Patients Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_qrs_plot


# Save the plot as jpg
ggsave(here("Results", db.name , "3_OsteoDx", paste0(EndoTxProstateCancer_inc_qrs_plot, ".jpg")), inc_qrs_plot, dpi=600, scale = 1, width = 12, height = 9)


print(paste0("- Analysis of all Endocrine Treatments in prostate cancer patients done"))
info(logger, "- Analysis of all Endocrine Treatments in prostate cancer patients done")