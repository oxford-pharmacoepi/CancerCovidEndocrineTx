# ============================================================================ #
#                       Incidence of Endocrine Treatments                      #
#                     in Breast and Prostate Cancer Cohorts                    #
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
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  #ageGroup = list(c(0,150)),
  sex = c("Both","Female","Male"),
  daysPriorHistory = 365,
  temporary = TRUE
)

count <-cohortCount(cdm$denominator)  

count2 <- cohortSet(cdm$denominator) 

Breast_strata_counts <- count %>% left_join(count2)

write.csv(Breast_strata_counts, file=here::here("Results", db.name, "2_EndocrineTxCancer", "Breast_strata_counts.csv"))
save(Breast_strata_counts, file=here::here("Results", db.name, "2_EndocrineTxCancer", "Breast_strata_counts.Rdata"))

print(paste0("- Got denominator_breast"))
info(logger, "- Got denominator_breast")



## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx in breast cancer populations"))
info(logger, "- Getting incidence endocrine tx in breast cancer populations")


IncTxBreast <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL, # add a filter here to specify which outcome cohorts to focus on specific to breast cancer
  interval = c("months", "quarters", "years"),
  #interval = "quarters",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf, 
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxBreast %>%
  glimpse()


save(IncTxBreast, file = here("Results", db.name, "2_EndocrineTxCancer", "IncTxBreast.RData"))

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


## ============ PLOTS FOR endocrine tx in breast cancer pop ================= ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in breast cancer populations denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in breast cancer populations denominator_breast 1")



# INCIDENCE IN YEARS FOR ALL AGE STRATA WITH 0 DAYS PRIOR HISTORY AND 90 DAYS WASHOUT

inc_yrs_plot <- IncTxBreast %>%  
  filter(denominator_cohort_id == 2) %>% # this denominator cohort is females only as males would not be prescribed these
 # filter(analysis_outcome_washout == 90) %>% 
  filter(analysis_interval == "years") %>%
  filter(outcome_cohort_name %in% c("AromataseInhibitors", "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED","Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED","Tamoxifen")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  #scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year", expand = c(0.05, 0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
    ggtitle("Incidence Rates of Endocrine Treatments in Years in Breast Cancer Patients in the year after diagnosis \nBefore and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_yrs_plot

analysis.name <- "endocrine_inBreastPop"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "2_EndocrineTxCancer", paste0(plotname, ".jpg")), inc_yrs_plot, dpi=600, scale = 1, width = 18, height = 9)



# INCIDENCE IN MONTHS FOR ALL AGE STRATA


inc_months_plot <- IncTxBreast %>%  
  filter(denominator_cohort_id == 2) %>% # this denominator cohort is females only
 # filter(analysis_outcome_washout == 90) %>% 
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("AromataseInhibitors", "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED","Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED","Tamoxifen")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  #scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="6 months", expand = c(0.05, 0.05)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Months in Breast Cancer Patients in the year after diagnosis \nBefore and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot


plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as jpg
ggsave(here("Results", db.name , "2_EndocrineTxCancer", paste0(plotname, ".jpg")), inc_months_plot, dpi=600, scale = 1, width = 18, height = 9)




# INCIDENCE IN QUARTERS FOR ALL AGE STRATA


inc_qrs_plot <- IncTxBreast %>%  
  filter(denominator_cohort_id == 2) %>% # this denominator cohort is for females only
  filter(analysis_interval == "quarters") %>%
  filter(outcome_cohort_name %in% c("AromataseInhibitors", "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED","Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED","Tamoxifen")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_qrs_plot <- 
  ggplot(inc_qrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
 # scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months", expand = c(0.05, 0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
    ggtitle("Incidence Rates of Endocrine Treatments in Quarters in Breast Cancer Patients in the year after diagnosis \nBefore and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_qrs_plot

plotname <- paste0(analysis.name, db.name, "_inc_qrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "2_EndocrineTxCancer", paste0(plotname, ".jpg")), inc_qrs_plot, dpi=600, scale = 1, width = 12, height = 9)






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
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = "Male",
  daysPriorHistory = 365,
  temporary = TRUE
)

count <-cohortCount(cdm$denominator)  

count2 <- cohortSet(cdm$denominator) 

Prostate_strata_counts <- count %>% left_join(count2)

write.csv(Prostate_strata_counts, file=here::here("Results", db.name, "2_EndocrineTxCancer", "Prostate_strata_counts.csv"))
save(Prostate_strata_counts, file=here::here("Results", db.name, "2_EndocrineTxCancer", "Prostate_strata_counts.RData"))

print(paste0("- Got denominator_Prostate"))
info(logger, "- Got denominator_Prostate")



## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence endocrine tx in Prostate cancer populations"))
info(logger, "- Getting incidence endocrine tx in Prostate cancer populations")


IncTxProstate <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL,
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf, 
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxProstate %>%
  glimpse()


save(IncTxProstate, file = here("Results", db.name, "2_EndocrineTxCancer", "IncTxProstate.RData"))

print(paste0("- Got incidence: endocrine tx in Prostate cancer populations"))
info(logger, "- Got incidence: endocrine tx in Prostate cancer populations")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: endocrine tx in Prostate cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: endocrine tx in Prostate cancer populations")

exportIncidencePrevalenceResults(resultList=list("incidence" = IncTxProstate), 
                                 zipName=paste0(db.name, "IncTxProstate"),
                                 outputFolder=here("Results", db.name, "2_EndocrineTxCancer")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in Prostate cancer populations"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in Prostate cancer populations")


## ============ PLOTS FOR endocrine tx in Prostate cancer pop ================= ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in Prostate cancer populations denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in Prostate cancer populations denominator_breast 1")



# INCIDENCE IN YEARS FOR ALL AGE STRATA 

inc_yrs_plot <- IncTxProstate %>%  
  filter(denominator_cohort_id == 1) %>% # this denominator cohort is for males only
  filter(analysis_interval == "years") %>%
  filter(outcome_cohort_name %in% c("First_generation_antiandrogens", "GNRH_Agonists_with1stGenADT_UPDATED","GNRH_Agonists","GNRH_LHRH_antagonists", "Second_generation_antiandrogens")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT_UPDATED" ~ "GnRH Agonists with First Generation Antiandrogens",
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
  #scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
    ggtitle("Incidence Rates of Endocrine Treatments in Years in Prostate Cancer Patients in the year after diagnosis \nBefore and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_yrs_plot

analysis.name <- "endocrine_inProstatePop"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "2_EndocrineTxCancer", paste0(plotname, ".jpg")), inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)



# INCIDENCE IN MONTHS FOR ALL AGE STRATA


inc_months_plot <- IncTxProstate %>%  
  filter(denominator_cohort_id == 1) %>% # this is the denominator cohort for males only
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name %in% c("First_generation_antiandrogens", "GNRH_Agonists_with1stGenADT_UPDATED","GNRH_Agonists","GNRH_LHRH_antagonists", "Second_generation_antiandrogens")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT_UPDATED" ~ "GnRH Agonists with First Generation Antiandrogens",
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
  #scale_y_continuous(limits = c(0, 150)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="6 months", expand = c(0.05,0.05)) +
  ggtitle("Incidence Rates of Endocrine Treatments in Months in Prostate Cancer Patients in the year after diagnosis \nBefore and After COVID-19 Lockdown)") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as jpg
ggsave(here("Results", db.name , "2_EndocrineTxCancer", paste0(plotname, ".jpg")), inc_months_plot, dpi=600, scale = 1, width = 18, height = 9)


# INCIDENCE IN QUARTERS FOR ALL AGE STRATA

inc_qrs_plot <- IncTxProstate %>%  
  filter(denominator_cohort_id == 1) %>% # denominator cohort for males only
  filter(analysis_interval == "quarters") %>%
  filter(outcome_cohort_name %in% c("First_generation_antiandrogens", "GNRH_Agonists_with1stGenADT_UPDATED","GNRH_Agonists","GNRH_LHRH_antagonists", "Second_generation_antiandrogens")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT_UPDATED" ~ "GnRH Agonists with First Generation Antiandrogens",
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
  #scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatments in Quarters in Prostate Cancer Patients in the year after diagnosis \nBefore and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_qrs_plot

plotname <- paste0(analysis.name, db.name, "_inc_qrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "2_EndocrineTxCancer", paste0(plotname, ".jpg")), inc_qrs_plot, dpi=600, scale = 1, width = 12, height = 9)




print(paste0("- Analysis of all Endocrine Treatments in prostate cancer patients done"))
info(logger, "- Analysis of all Endocrine Treatments in prostate cancer patients done")


