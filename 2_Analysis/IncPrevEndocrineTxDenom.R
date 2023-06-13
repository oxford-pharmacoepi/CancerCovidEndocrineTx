# ============================================================================ #
#                       Incidence of Endocrine Treatments                      #
#                     in Breast and Prostate Cancer Cohorts                    #
#                              Nicola Barclay                                  #
#                                08-06-2023                                    #
# ============================================================================ #

## ============== ENDOCRINE TREATMENTS IN DENOMINATOR POP CANCER ============ ##

print(paste0("- 2. Incidence of Endocrine Treatments in Denominator Cohort"))
info(logger, "- 2. Incidence of Endocrine Treatments in Denominator Cohort")

## =================== SET THE DENOMINATOR COHORTS =================== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  ageGroup = list(c(0,150), c(0,19), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both", "Male", "Female"),
  daysPriorHistory = 0,
  temporary = TRUE
)

#cdm$denominator %>% tally()

cohortCount(cdm$denominator)

cohortSet(cdm$denominator) 

# #dpop <- cdm$denominator %>%
#   # collect() %>%
#   left_join(cohortSet(cdm$denominator))
# 
# dpop %>%
#   glimpse()
# 
# dpop %>%
#   group_by(cohort_definition_id, age_group) %>%
#   tally()
# 
# View attrition table
# cohortAttrition(cdm$denominator)

print(paste0("- Got denominator"))
info(logger, "- Got denominator")


## ================ INCIDENCE OF ENDOCRINE TREATMENTS ======================= ##

print(paste0("- 2. Incidence of Endocrine Treatments in general population"))
info(logger, "- 2. Incidence of Endocrine Treatments in general population")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = outcome_cohorts_1$cohortId, 
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = c(0, NULL, 90), 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

inc %>%
  glimpse()


save(inc, file = here("Results", db.name, "1_EndocrineTxDenom", "IncTxDenom.RData"))


print(paste0("- Got incidence: endocrine tx in general population"))
info(logger, "- Got incidence: endocrine tx in general population")



## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence: endocrine tx in general population"))
info(logger, "- Exporting incidence: endocrine tx in general population")

exportIncidencePrevalenceResults(resultList=list("inc" = inc), 
                                 zipName=paste0(db.name, "IncTxDenom"),
                                 outputFolder=here("Results", db.name, "1_EndocrineTxDenom")) 

print(paste0("- Exported incidence: endocrine tx in general population"))
info(logger, "- Exported incidence: endocrine tx in general population")


## ===================== PLOTS FOR denominator_breast POP == 1 ===================== ##


print(paste0("- Plotting incidence: endocrine tx in general population"))
info(logger, "- Plotting incidence: endocrine tx in general population")


# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- inc %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 90) %>% 
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  ggtitle("Incidence Rates of Endocrine Treatments for Breast or Prostate Cancer in Years in the General Population Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 12))

inc_yrs_plot

analysis.name <- "endocrine"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "1_EndocrineTxDenom", paste0(plotname, ".jpg")), inc_yrs_plot, dpi=600, scale = 1, width = 12, height = 9)



# INCIDENCE IN MONTHS FOR ALL AGE STRATA

inc_months_plot <- inc %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 90) %>% 
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="6 months") +
  ggtitle("Incidence Rates of Endocrine Treatments for Breast or Prostate Cancer in Months in the General Population Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 12))

inc_months_plot

plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as jpg
ggsave(here("Results", db.name , "1_EndocrineTxDenom", paste0(plotname, ".jpg")), inc_months_plot, dpi=600, scale = 1, width = 12, height = 9)



# INCIDENCE IN QUARTERS FOR ALL AGE STRATA

inc_qrs_plot <- inc %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_outcome_washout == 0) %>% 
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_qrs_plot <- 
  ggplot(inc_qrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months") +
  ggtitle("Incidence Rates of Endocrine Treatments for Breast or Prostate Cancer in Quarters in the General Population Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 12))

inc_qrs_plot

plotname <- paste0(analysis.name, db.name, "_inc_qrs")


# Save the plot as jpg
ggsave(here("Results", db.name , "1_EndocrineTxDenom", paste0(plotname, ".jpg")), inc_qrs_plot, dpi=600, scale = 1, width = 12, height = 9)


print(paste0("- Analysis of all Endocrine Treatments in general population done"))
info(logger, "- Analysis of all Endocrine Treatments in general population done")