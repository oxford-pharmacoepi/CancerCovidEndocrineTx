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


count <-cohortCount(cdm$denominator)  

count2 <- cohortSet(cdm$denominator) 

Breast_AI_counts <- count %>% left_join(count2)

write.csv(Breast_AI_counts, file=here::here("Results", db.name, "3_OsteoDx", "Breast_AI_counts.csv"))
save(Breast_AI_counts, file=here::here("Results", db.name, "3_OsteoDx", "Breast_AI_counts.RData"))


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
  outcomeWashout = Inf, 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxOutcomesBreastAI %>%
  glimpse()

IncTxOutcomesBreastAIAtt <- IncTxOutcomesBreastAI %>%
  incidenceAttrition()

save(IncTxOutcomesBreastAI, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesBreastAI.RData"))
write.csv(IncTxOutcomesBreastAIAtt, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesBreastAI_attrition_estimates.csv"))



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
  filter(denominator_cohort_id == 2) %>%
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
  scale_x_date(date_labels="%Y",date_breaks  ="1 year", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
    ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Years in Breast Cancer Patients one year after diagnosis \non Aromatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_yrs_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_yrs_plot, dpi=600, scale = 1, width = 15, height = 10)




# INCIDENCE IN MONTHS FOR ALL AGE STRATA

inc_months_plot <- IncTxOutcomesBreastAI %>%  
  filter(denominator_cohort_id == 2) %>%
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
  scale_y_continuous(limits = c(0, 20000)) +
  scale_x_date(date_labels="%b %Y",date_breaks="6 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen())+
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Months in Breast Cancer Patients one year after diagnosis \non Aromatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_months_plot, dpi=600, scale = 1, width = 15, height = 10)




# INCIDENCE IN QUARTERS FOR ALL AGE STRATA


inc_qrs_plot <- IncTxOutcomesBreastAI %>%  
  filter(denominator_cohort_id == 2) %>%
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
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Quarters in Breast Cancer Patients one year after diagnosis \non Aromatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_qrs_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_qrs")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_qrs_plot, dpi=600, scale = 1, width = 15, height = 10)



# INCIDENCE IN QUARTERS FOR ALL AGE STRATA WITHOUT DENOSUMAB AND BONE FRACTURE



dateVec <- seq(from = as.Date("2017-01-01"), to = as.Date("2022-06-01"), by = "3 months")

break.vec <- c(as.Date("2017-01-01"),
               seq(from = as.Date("2017-04-01"), to = as.Date("2022-04-01"),
                   by = "3 months"),
               as.Date("2022-07-01"))


# INCIDENCE IN MONTHS FOR ALL AGE STRATA WITHOUT DENOSUMAB AND BONE FRACTURE

break.vec <- c(as.Date("2017-01-01"),
               seq(from = as.Date("2017-04-01"), to = as.Date("2022-04-01"),
                   by = "3 months"),
               as.Date("2022-07-01"))

inc_months_plot1 <- IncTxOutcomesBreastAI %>%  
  filter(denominator_cohort_id == 2) %>%
  filter(analysis_interval == "months") %>%
  filter(outcome_cohort_name == c("Bisphosphonates","Osteopenia", "Osteoporosis")) %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis")) %>% 
  
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot1, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, 20000)) +
  #scale_x_date(date_labels="%b %Y",date_breaks="3 months", limits = c(min(dateVec), max=max(dateVec)), expand=c(0.05,0)) +
  scale_x_date(date_labels="%b %Y",breaks=dateVec, expand=c(0.05,0)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen())+
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Months in Breast Cancer Patients one year after diagnosis \non Aromatase Inhibitors Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

analysis.name <- "txOutcomes_inBreastAIPop"
plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_months_plot, dpi=600, scale = 1, width = 15, height = 10)



print(paste0("- Analysis of all AI Treatment-Related Outcomes in breast cancer patients done"))
info(logger, "- Analysis of all AI Treatment-Related Outcomes in breast cancer patients done")







## ============== SET THE BREAST CANCER TAMOXIFEN TREATMENT STRATA DENOMINATOR COHORTS ========== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  strataTable = strata_table_name_2,
  strataCohortId = 2,
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = c("Both","Female","Male"),
  daysPriorHistory = 365,
  temporary = TRUE
)

count <-cohortCount(cdm$denominator)  # CHECK NAME OF OUTCOME COHORT IN TABLE AS MIGHT NOT LIKE BRACKETS

count2 <- cohortSet(cdm$denominator) 

Breast_tamoxifen_counts <- count %>% left_join(count2)

write.csv(Breast_tamoxifen_counts, file=here::here("Results", db.name, "3_OsteoDx", "Breast_tamoxifen_counts.csv"))
save(Breast_tamoxifen_counts, file=here::here("Results", db.name, "3_OsteoDx", "Breast_tamoxifen_counts.RData"))


print(paste0("- Got denominator_breast_TX"))
info(logger, "- Got denominator_breast_TX")



## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence treatment related outcomes in breast cancer populations on Tamoxifen"))
info(logger, "- Getting incidence treatment related outcomes in breast cancer populations on Tamoxifen")


IncTxOutcomesBreastTAM <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = NULL, # add a filter here to specify which outcome cohorts to focus on specific to breast cancer
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf, 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxOutcomesBreastTAM %>%
  glimpse()

IncTxOutcomesBreastTAMAtt <- IncTxOutcomesBreastTAM %>%
  incidenceAttrition()

save(IncTxOutcomesBreastAI, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesBreastAI.RData"))
write.csv(IncTxOutcomesBreastTAMAtt, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesBreastTAM_attrition_estimates.csv"))



save(IncTxOutcomesBreastTAM, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesBreastTAM.RData"))


print(paste0("- Got incidence: treatment related outcomes in breast cancer populations on TAM"))
info(logger, "- Got incidence: treatment related outcomes in breast cancer populations on TAM")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: treatment related outcomes in breast cancer populations on TAM"))
info(logger, "- Exporting incidence and Prevalence results: treatment related outcomes in breast cancer populations on TAM")

exportIncidencePrevalenceResults(resultList=list("incidence" = IncTxOutcomesBreastTAM), 
                                 zipName=paste0(db.name, "IncTxOutcomesBreastTAM"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in breast cancer populations on TAM"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in breast cancer populations on TAM")


## ============ PLOTS FOR endocrine tx in breast cancer pop  on TAMOXIFEN ================= ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in breast cancer populations  on TAM denominator_breast 1"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in breast cancer populations  on TAM denominator_breast 1")



# INCIDENCE IN YEARS FOR ALL AGE STRATA WITH 0 DAYS PRIOR HISTORY AND 90 DAYS WASHOUT

inc_yrs_plot <- IncTxOutcomesBreastTAM %>%  
  filter(denominator_cohort_id == 2) %>%
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
  scale_x_date(date_labels="%Y",date_breaks  ="1 year", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Years in Breast Cancer Patients one year after diagnosis \non Tamoxifen Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_yrs_plot

analysis.name <- "txOutcomes_inBreastTAMPop"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_yrs_plot, dpi=600, scale = 1, width = 15, height = 10)




# INCIDENCE IN MONTHS FOR ALL AGE STRATA

inc_months_plot <- IncTxOutcomesBreastTAM %>%  
  filter(denominator_cohort_id == 2) %>%
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
  scale_y_continuous(limits = c(0, 20000)) +
  scale_x_date(date_labels="%b %Y",date_breaks="6 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen())+
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Months in Breast Cancer Patients one year after diagnosis \non Tamoxifen Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

analysis.name <- "txOutcomes_inBreastTAMPop"
plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_months_plot, dpi=600, scale = 1, width = 15, height = 10)




# INCIDENCE IN QUARTERS FOR ALL AGE STRATA


inc_qrs_plot <- IncTxOutcomesBreastTAM %>%  
  filter(denominator_cohort_id == 2) %>%
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
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Quarters in Breast Cancer Patients one year after diagnosis  \non Tamoxifen Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_qrs_plot

analysis.name <- "txOutcomes_inBreastTAMPop"
plotname <- paste0(analysis.name, db.name, "_inc_qrs")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_qrs_plot, dpi=600, scale = 1, width = 15, height = 10)




print(paste0("- Analysis of all Endocrine Treatment-Related Outcomes in breast cancer patients done"))
info(logger, "- Analysis of all Endocrine Treatment-Related Outcomes in breast cancer patients done")



## =========================== PROSTATE CANCER ================================ ##

print(paste0("- 1. Incidence and Prevalence of Endocrine Treatments in Prostate Cancer Cohort"))
info(logger, "- 1. Incidence and Prevalence of Endocrine Treatments in Prostate Cancer Cohort")

## ============== SET THE PROSTATE CANCER STRATA DENOMINATOR COHORTS ========== ##

print(paste0("- Getting denominator population"))
info(logger, "- Getting denominator population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  cohortDateRange = as.Date(c("2017-01-01","2022-07-01")),
  strataTable = strata_table_name_2,
  strataCohortId = 3,
  ageGroup = list(c(0,150), c(20,39), c(40,59), c(60,79), c(80,150)),
  sex = "Male",
  daysPriorHistory = 365,
  temporary = TRUE
)

count <- cohortCount(cdm$denominator) # CHECK NAME OF OUTCOME COHORT IN TABLE AS MIGHT NOT LIKE BRACKETS

count2 <- cohortSet(cdm$denominator) 

Prostate_endocrine_counts <- count %>% left_join(count2)

write.csv(Prostate_endocrine_counts, file=here::here("Results", db.name, "3_OsteoDx", "Prostate_endocrine_counts.csv"))
save(Prostate_endocrine_counts, file=here::here("Results", db.name, "3_OsteoDx", "Prostate_endocrine_counts.RData"))


print(paste0("- Got denominator_prostate"))
info(logger, "- Got denominator_prostate")



## ======================== CALCULATE INCIDENCE ============================= ##

print(paste0("- Getting incidence treatment related outcomes in prostate cancer population"))
info(logger, "- Getting incidence treatment related outcomes in prostate cancer population")


IncTxOutcomesProstate <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = outcome_table_name_2, 
  outcomeCohortId = NULL, # add a filter here to specify which outcome cohorts to focus on specific to breast cancer
  interval = c("months", "quarters", "years"),
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf, 
  repeatedEvents = TRUE,
  minCellCount = 5,
  temporary = TRUE,
  returnParticipants = FALSE
)

IncTxOutcomesProstate %>%
  glimpse()

IncTxOutcomesProstateAtt <- IncTxOutcomesProstate %>%
  incidenceAttrition()

write.csv(IncTxOutcomesProstateAtt, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesProstateAtt_attrition_estimates.csv"))


save(IncTxOutcomesProstate, file = here("Results", db.name, "3_OsteoDx", "IncTxOutcomesProstate.RData"))



print(paste0("- Got incidence: treatment related outcomes in Prostate Cancer Cohort on endocrine tx"))
info(logger, "- Got incidence: treatment related outcomes in Prostate Cancer Cohort on endocrine tx")


## ======== EXPORT ALL INCIDENCE AND PREVALENCE RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: treatment related outcomes in Prostate Cancer Cohort on endocrine tx"))
info(logger, "- Exporting incidence and Prevalence results: treatment related outcomes in Prostate Cancer Cohort on endocrine tx")

exportIncidencePrevalenceResults(resultList=list("incidence" = IncTxOutcomesProstate), 
                                 zipName=paste0(db.name, "IncTxOutcomesProstate"),
                                 outputFolder=here("Results", db.name, "3_OsteoDx")) 

print(paste0("- Exported incidence and prevalence results: endocrine tx in Prostate Cancer Cohort on endocrine tx"))
info(logger, "- Exported incidence and prevalence results: endocrine tx in Prostate Cancer Cohort on endocrine tx")


## ============ PLOTS FOR endocrine tx in Prostate Cancer Cohort on endocrine tx ================= ##


print(paste0("- Plotting incidence and prevalence results: endocrine tx in Prostate Cancer Cohort on endocrine tx"))
info(logger, "- Plotting incidence and prevalence results: endocrine tx in Prostate Cancer Cohort on endocrine tx")



# INCIDENCE IN YEARS FOR ALL AGE STRATA WITH 0 DAYS PRIOR HISTORY AND 90 DAYS WASHOUT

inc_yrs_plot <- IncTxOutcomesProstate %>%  
  filter(denominator_cohort_id == 1) %>%
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
  scale_x_date(date_labels="%Y",date_breaks  ="1 year", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Years in Prostate Cancer Patients one year after diagnosis \non Endocrine Treatment Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_yrs_plot

analysis.name <- "txOutcomes_inProstatePop"
plotname <- paste0(analysis.name, db.name, "_inc_yrs")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_yrs_plot, dpi=600, scale = 1, width = 15, height = 10)




# INCIDENCE IN MONTHS FOR ALL AGE STRATA

inc_months_plot <- IncTxOutcomesProstate %>%  
  filter(denominator_cohort_id == 1) %>%
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
  scale_x_date(date_labels="%b %Y",date_breaks  ="6 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Months in Prostate Cancer Patients one year after diagnosis \non Endocrine Treatment Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

analysis.name <- "txOutcomes_inProstatePop"
plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_months_plot, dpi=600, scale = 1, width = 15, height = 10)


# INCIDENCE IN MONTHS FOR ALL AGE STRATA WITHOUT DENOSUMAB, BONE FRACTURES


inc_months_plot <- IncTxOutcomesProstate %>%  
  filter(denominator_cohort_id == 1) %>%
  filter(analysis_interval == "months") %>%
  mutate(outcome = case_when(outcome_cohort_name == "Bisphosphonates" ~ "Bisphosphonates",
                             outcome_cohort_name == "Osteopenia" ~ "Osteopenia",
                             outcome_cohort_name == "Osteoporosis" ~ "Osteoporosis")) %>% 
  filter(outcome %in% c("Bisphosphonates","Osteopenia", "Osteoporosis")) %>%
  as.data.frame()

inc_months_plot <- 
  ggplot(inc_months_plot, aes(x = incidence_start_date, y=incidence_100000_pys,
                              ymin = incidence_100000_pys_95CI_lower,
                              ymax = incidence_100000_pys_95CI_upper, color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_labels="%b %Y",breaks=dateVec, expand=c(0.05,0)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Months in Prostate Cancer Patients one year after diagnosis \non Endocrine Treatment Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_months_plot

analysis.name <- "txOutcomes_inProstatePop"
plotname <- paste0(analysis.name, db.name, "_inc_months")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_months_plot, dpi=600, scale = 1, width = 15, height = 10)



# INCIDENCE IN QUARTERS FOR ALL AGE STRATA


inc_qrs_plot <- IncTxOutcomesProstate %>%  
  filter(denominator_cohort_id == 1) %>%
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
  scale_x_date(date_labels="%b %Y",date_breaks  ="3 months", expand = c(0.05,0.05)) +
  facet_wrap(~outcome, nrow=2, scales = "free_y", labeller = label_wrap_gen()) +
  ggtitle("Incidence Rates of Endocrine Treatment-Related Outcomes in Quarters in Prostate Cancer Patients one year after diagnosis \non Endocrine Treatments Before and After COVID-19 Lockdown") +
  labs(colour = "Endocrine Treatment", x="Time" , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red") +
  theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

inc_qrs_plot

analysis.name <- "txOutcomes_inProstatePop"
plotname <- paste0(analysis.name, db.name, "_inc_qrs")

# Save the plot as tiff
ggsave(here("Results", db.name , "3_OsteoDx", paste0(plotname, ".tiff")), inc_qrs_plot, dpi=600, scale = 1, width = 15, height = 10)



print(paste0("- Analysis of all Endocrine Treatment-Related Outcomes in Prostate Cancer Cohort on endocrine tx done"))
info(logger, "- Analysis of all Endocrine Treatment-Related Outcomes in Prostate Cancer Cohort on endocrine tx done")





