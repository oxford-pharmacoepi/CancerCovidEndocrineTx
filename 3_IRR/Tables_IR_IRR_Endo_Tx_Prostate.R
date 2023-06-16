# ============================================================================ #
#     Create tables with incidence incidence rates and incidence rate ratios   #
#                   compared to the pre-covid reference period                 #
#                  for endocrine treatments in prostate cancer                   #
#                                                                              #
#                              Nicola Barclay                                  #
#                                16-06-2023                                    #
# ============================================================================ #


renv::restore() # this should prompt you to install the various packages required for the study

# packages
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
require(foreign)
require(MASS)
library(tsibble)
library(graphics)
library(feasts)
library(magrittr)
library(epiR)
library(fmsb)
library(epitools)
library(flextable)
library(data.table)

load(here("0_DataPrep", "Data", "inc_data_endo_tx_in_prostate.RData"))

# Periods-----------------

IR.age <- inc_data_endo_tx_in_prostate %>% drop_na(outcome)

Sys.setlocale("LC_TIME", "English")

IR.age$covid <- as.factor(IR.age$covid)
IR.age$covid <-relevel(IR.age$covid, "Pre-COVID")




# ================ CALCULATE OBSERVED INCIDENCE RATES FOR EACH ENDOCRINE TREATMENT FOR prostate CANCER OVER PERIODS ==================== #
# ======================== AGE STRATIFICATION =========================== #  

age <-IR.age %>% filter(denominator_sex == "Male") %>% group_by(covid, outcome, denominator_age_group) %>% summarise(events_t = sum(events),person_months_at_risk = sum(months),)



ir_all <- age %>% arrange(covid, outcome)%>% relocate(denominator_age_group, .after = outcome)
  
ir1_all <-as.matrix(ir_all[,4:5])
ci_all <- round(epi.conf(ir1_all, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci_all <- cbind(ir_all, ci_all)
ir_ci_all <- ir_ci_all %>% 
  mutate(ir = paste0(paste(est), " (",paste(lower), " to ", paste(upper), ")"))%>%
  dplyr::select(covid, outcome,  denominator_age_group, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome, denominator_age_group)


write.csv(ir_ci_all, file=here("3_IRR", "Observed_incidence_rates_Endo_Tx_prostate.csv"))
save(ir_ci_all, file=here("3_IRR", "Observed_incidence_rates_Endo_Tx_prostate.RData"))


# add combined periods post-lockdown
age.post <-IR.age%>% 
  filter(months.since.start >=31)%>%
  group_by(outcome,denominator_age_group) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)

ir_post <- age.post %>% arrange(outcome)

ir1_post <-as.matrix(ir_post[,3:4])
ci_post <- round(epi.conf(ir1_post, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci_post <- cbind(ir_post, ci_post)
ir.post_ci <- ir_ci_post %>% 
  mutate(ir = paste0(paste(est)," (", paste(lower), " to ", paste(upper), ")"))%>%
  mutate(covid="Post-lockdown")%>%
  dplyr::select(covid,outcome, denominator_age_group, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)


write.csv(ir.post_ci, file=here("3_IRR", "Observed_incidence_rates_post_lockdown_EndoTx_prostate.csv"))
save(ir.post_ci, file=here("3_IRR", "Observed_incidence_rates_post_lockdown_EndoTx_prostate.RData"))


# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci_all, ir.post_ci)

write.csv(ir_ci_pre_post, file=here("3_IRR", "Observed_incidence_rates_pre_post_lockdown_EndoTx_prostate.csv"))
save(ir_ci_pre_post, file=here("3_IRR", "Observed_incidence_rates_pre_post_lockdown_EndoTx_prostate.RData"))

# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 


ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[, c(1, 2, 3, 6, 7, 8, 9, 4, 5, 10)]
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename("Age Group" = denominator_age_group,
                                                        "Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (July 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-lockdown 1 (July 2020-Dec 2021)" = "Post-lockdown1",
                                                        "Second lockdown (Nov 2020-Dec 2020)" = "Second lockdown", 
                                                        "Third lockdown (Jan 2021-Feb 2021)" = "Third lockdown",
                                                        "Easing of restrictions (March 2021-June 2021" = "Easing of restrictions", 
                                                        "Legal restrictions removed (July 2021-Dec 2021)"= "Legal restrictions removed")


Pretty_observed_IR_results_table <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of prostate cancer endocrine treatments in each of the time periods, stratified by age") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here("3_IRR", "Observed incidence_rates_ir_ci_pre_post_pivot_EndoTx_prostate.RData"))
write.csv(ir_ci_pre_post_pivot, file=here("3_IRR", "Observed incidence_rates_ir_ci_pre_post_pivot_EndoTx_prostate.csv"))

save_as_docx('Pretty_observed_IR_results_table' = Pretty_observed_IR_results_table, path=here("3_IRR", "Observed incidence_rates_EndoTx_prostate.docx"))





# ================ CALCULATE IRR FOR EACH ENDOCRINE TREATMENT FOR prostate CANCER OVER PERIODS ==================== #

# ======================== AGE STRATIFICATION =========================== #  

load(here("0_DataPrep", "Data", "inc_data_endo_tx_in_prostate.RData"))

# This code calculates the IRR for each of the prostate cancer endocrine treatments
# over a loop for each period of interest
# and loops over age categories

# first filter out prostate treatments:
IR.age <- inc_data_endo_tx_in_prostate %>% drop_na(outcome)

############ UPTO HERE############


# the reason why there is no data for denominator cohort id for any other id is that the results were obscured for all these categories
# need to check this in the original file and the data prep file
IR.age <- IR.age %>% filter(  denominator_cohort_id == 5)

# CREATE A NEW COLUMN OF Age and Sex groups
IR.age <- IR.age %>% mutate(Age_sex = case_when(grepl("5", denominator_cohort_id) ~ "Male; 80-150",
                                                grepl("5", denominator_cohort_id) ~ "Female; 20-39",
                                                grepl("8", denominator_cohort_id) ~ "Female; 40-59",
                                                grepl("11", denominator_cohort_id) ~ "Female; 60-79",
                                                grepl("14", denominator_cohort_id) ~ "Female; 80-150"))



names_cohort_id = names(table(IR.age$Age_sex))
number_cohort_id = length(names_cohort_id)

IR <- IR.age

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
rateratios <- list()

for (id in names_cohort_id){
  for (y in 1:length(outcome)){
    working.outcome <- outcome[y]
    vector <- NULL # a vector to place the values from the loop
    for(z in 1:length(periods)){
      working.period <- periods[z]
      working.data <- IR %>%
        filter(Age_sex==id)%>%
        filter(outcome==working.outcome)%>%
        filter(covid==working.period) %>%
        mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
        group_by(ref)%>% 
        summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
        mutate(periods = paste(working.period))%>%
        mutate(outcome= paste(working.outcome))
      
      events <- c(working.data%>%dplyr::select(events_t)%>%pull())
      pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
      
      vector <- vector %>%
        union_all(
          tibble(
            events = events, person_time = pt, period = periods[z]
          )
        )
      
    }
    count <- paste(outcome[y], id, sep = ";")
    if (dim(vector)[1] > 1) {
      rateratios[[count]] <- rateratio(as.matrix(vector[,1:2], y=NULL))$measure %>% bind_cols(vector[,3])
    }
    
  }
}

rateratios_table <- bind_rows(rateratios, .id = "age_sex")


# LAYOUT THE TABLE HOW YOU WANT IT

rate_ratios_table_formatted <- rateratios_table %>%  mutate_if(is.numeric, round, digits=2)

# combine cis with the estimate
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 


# CREATE A NEW COLUMN OF CANCER TYPES
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate("Endocrine Treatment" = case_when(grepl("Aromatase Inhibitors with GnRH Agonists Or Antagonists", age_sex) ~ "Aromatase Inhibitors with GnRH Agonists Or Antagonists",
                                                                                                        grepl("Aromatase Inhibitors;Female", age_sex) ~ "Aromatase Inhibitors",
                                                                                                        grepl("Tamoxifen with GnRH Agonists Or Antagonists", age_sex) ~ "Tamoxifen with GnRH Agonists Or Antagonists",
                                                                                                        grepl("Tamoxifen;Female", age_sex) ~ "Tamoxifen"))



# CREATE A NEW COLUMN OF Age groups - THIS BIT OF CODE MERGES 80-150 AND 0-150 BECAUSE IT READS 80-150 WHEN IT READS 0-150
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate("Age Group" = case_when(grepl(" 0-150", age_sex) ~ "0-150",
                                                                                              grepl("20-39", age_sex) ~ "20-39",
                                                                                              grepl("40-59", age_sex) ~ "40-59",
                                                                                              grepl("60-79", age_sex) ~ "60-79",
                                                                                              grepl("80-150", age_sex) ~ "80-150"))



# remove superfluous columns of cis
rate_ratios_table_formatted <- rate_ratios_table_formatted[-c(3,4)]

# Re-order columns 
rate_ratios_table_formatted <- rate_ratios_table_formatted[c(4,5,3,2)]



# pivot - 
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% pivot_wider(names_from = period, values_from = estimate) %>% arrange(`Endocrine Treatment`, `Age Group`)
#drop pre-lockdown reference column and re-order periods
rate_ratios_table_formatted <- rate_ratios_table_formatted[c(1,2,5,4,6,7,8,9)]


#### Save IRR
write.csv(rate_ratios_table_formatted, file=here::here("3_IRR", "Observed IRR_EndoTx_prostate.csv"))
save(rate_ratios_table_formatted, file=here::here("3_IRR", "Observed IRR_EndoTx_prostate.RData"))

#### Make pretty table for prostate cancer

Pretty_IRR_table_EndoTxprostate<- flextable(rate_ratios_table_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of endocrine treatments in prostate cancer patients over the lockdown periods compared to pre-COVID period, stratified by sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_EndoTxprostate' = Pretty_IRR_table_EndoTxprostate, path=here("3_IRR", "Pretty_IRR_table_EndoTxprostate.docx"))