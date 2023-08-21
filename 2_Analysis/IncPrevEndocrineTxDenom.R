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
  daysPriorHistory = 365,
  temporary = TRUE
)


count <-cohortCount(cdm$denominator)  

count2 <- cohortSet(cdm$denominator) 

Denominator_counts <- count %>% left_join(count2)

write.csv(Denominator_counts, file=here::here("Results", db.name, "1_EndocrineTxDenom", "Denominator_counts.csv"))
save(Denominator_counts, file = here("Results", db.name, "1_EndocrineTxDenom", "Denominator_counts.RData"))




print(paste0("- Got denominator"))
info(logger, "- Got denominator")


## ================ INCIDENCE OF ENDOCRINE TREATMENTS ======================= ##

print(paste0("- 2. Incidence of Endocrine Treatments in general population"))
info(logger, "- 2. Incidence of Endocrine Treatments in general population")


inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_1, 
  outcomeCohortId = NULL, 
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
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


## ===================== PLOTS  ===================== ##


print(paste0("- Plotting incidence: endocrine tx in general population"))
info(logger, "- Plotting incidence: endocrine tx in general population")


# INCIDENCE IN YEARS FOR ALL AGE STRATA

inc_yrs_plot <- inc %>%  
  filter(denominator_cohort_id == 1) %>% # this is the denominator cohort with 365 days prior history
  filter(analysis_interval == "years") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT_UPDATED" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_yrs_plot <- 
  ggplot(inc_yrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatments for Breast or Prostate Cancer in Years \nin the General Population Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_yrs_plot

analysis.name <- "endocrine"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as jpg
ggsave(here("Results", db.name , "1_EndocrineTxDenom", paste0(plotname, ".jpg")), inc_yrs_plot, dpi=900, scale = 1, width = 18, height = 9)



# INCIDENCE IN MONTHS FOR ALL AGE STRATA

inc_months_plot <- inc %>%  
  filter(denominator_cohort_id == 1) %>% # this is the denominator cohort with 365 days prior history
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT_UPDATED" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="6 months",expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatments for Breast or Prostate Cancer in Months \nin the General Population Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as jpg
ggsave(here("Results", db.name , "1_EndocrineTxDenom", paste0(plotname, ".jpg")), inc_months_plot, dpi=900, scale = 1, width = 18, height = 9)



# INCIDENCE IN QUARTERS FOR ALL AGE STRATA

inc_qrs_plot <- inc %>%  
  filter(denominator_cohort_id == 1) %>% # this is the denominator cohort with 365 days prior history
  filter(analysis_interval == "quarters") %>%
  mutate(outcome = case_when(outcome_cohort_name == "AromataseInhibitors" ~ "Aromatase Inhibitors",
                             outcome_cohort_name == "AromataseInhibitors_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "First_generation_antiandrogens" ~ "First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists_with1stGenADT_UPDATED" ~ "GnRH Agonists with First Generation Antiandrogens",
                             outcome_cohort_name == "GNRH_Agonists" ~ "GnRH Agonists",
                             outcome_cohort_name == "GNRH_LHRH_antagonists" ~ "GnRH Antagonists",
                             outcome_cohort_name == "Second_generation_antiandrogens" ~ "Second Generation Antiandrogens",
                             outcome_cohort_name == "Tamoxifen_withGnRHAgonistsOrAntagonists_UPDATED" ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                             outcome_cohort_name == "Tamoxifen" ~ "Tamoxifen")) %>% 
  as.data.frame()

inc_qrs_plot <- 
  ggplot(inc_qrs_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                           ymin = incidence_100000_pys_95CI_lower,
                           ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatments for Breast or Prostate Cancer in Quarters \nin the General Population Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_qrs_plot

plotname <- paste0(analysis.name, db.name, "_inc_qrs")


# Save the plot as jpg
ggsave(here("Results", db.name , "1_EndocrineTxDenom", paste0(plotname, ".jpg")), inc_qrs_plot, dpi=900, scale = 1, width = 18, height = 9)



print(paste0("- Analysis of all Endocrine Treatments in general population done"))
info(logger, "- Analysis of all Endocrine Treatments in general population done")
