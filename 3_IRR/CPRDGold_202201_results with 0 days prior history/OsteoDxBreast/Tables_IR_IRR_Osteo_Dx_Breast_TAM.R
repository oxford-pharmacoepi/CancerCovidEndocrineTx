# ============================================================================ #
#     Create tables with incidence incidence rates and incidence rate ratios   #
#                   compared to the pre-covid reference period                 #
#                  for oste- treatment related outcomes in                     #
#                  breast cancer patients on tamoxifen                         #
#                                                                              #
#                              Nicola Barclay                                  #
#                                20-06-2023                                    #
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

load(here("0_DataPrep", "Data", "inc_data_OsteoDx_in_breastTAM.RData"))

# Periods-----------------

IR.age <- inc_data_OsteoDx_in_breastTAM

Sys.setlocale("LC_TIME", "English")

IR.age$covid <- as.factor(IR.age$covid)
IR.age$covid <-relevel(IR.age$covid, "Pre-COVID")




# ================ CALCULATE OBSERVED INCIDENCE RATES FOR EACH TREATMENT RELATED OUTCOME FOR BREAST CANCER OVER PERIODS ==================== #
# ======================== AGE STRATIFICATION =========================== #  

age <-IR.age %>% filter(denominator_sex == "Female") %>% group_by(covid, outcome, denominator_age_group) %>% summarise(events_t = sum(events),person_months_at_risk = sum(months),)



ir_all <- age %>% arrange(covid, outcome)%>% relocate(denominator_age_group, .after = outcome)
  
ir1_all <-as.matrix(ir_all[,4:5])
ci_all <- round(epi.conf(ir1_all, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci_all <- cbind(ir_all, ci_all)
ir_ci_all <- ir_ci_all %>% 
  mutate(ir = paste0(paste(est), " (",paste(lower), " to ", paste(upper), ")"))%>%
  dplyr::select(covid, outcome,  denominator_age_group, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome, denominator_age_group)


write.csv(ir_ci_all, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed_incidence_rates_OsteoDx_breastTAM.csv"))
save(ir_ci_all, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed_incidence_rates_OsteoDx_breastTAM.RData"))


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


write.csv(ir.post_ci, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed_incidence_rates_post_lockdown_OsteoDx_breastTAM.csv"))
save(ir.post_ci, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed_incidence_rates_post_lockdown_OsteoDx_breastTAM.RData"))


# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci_all, ir.post_ci)

write.csv(ir_ci_pre_post, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed_incidence_rates_pre_post_lockdown_OsteoDx_breastTAM.csv"))
save(ir_ci_pre_post, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed_incidence_rates_pre_post_lockdown_OsteoDx_breastTAM.RData"))

# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 


ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename("Age Group" = denominator_age_group,
                                                        "Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (July 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-lockdown 1 (July 2020-Dec 2021)" = "Post-lockdown1")


Pretty_observed_IR_results_table <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of treatment-related outcomes in breast cancer patients on tamoxifen in each of the time periods, stratified by age") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed incidence_rates_ir_ci_pre_post_pivot_OsteoDx_breastTAM.RData"))
write.csv(ir_ci_pre_post_pivot, file=here("3_IRR", db.name, "OsteoDxBreast",  "Observed incidence_rates_ir_ci_pre_post_pivot_OsteoDx_breastTAM.csv"))

save_as_docx('Pretty_observed_IR_results_table' = Pretty_observed_IR_results_table, path=here("3_IRR", db.name, "OsteoDxBreast",  "Observed incidence_rates_OsteoDx_breastTAM.docx"))





# ================ CALCULATE IRR FOR EACH TREATMENT-RELATED OUTCOME FOR BREAST CANCER OVER PERIODS ==================== #

# ======================== AGE STRATIFICATION =========================== #  

load(here("0_DataPrep", "Data", "inc_data_OsteoDx_in_breastTAM.RData"))

# This code calculates the IRR for each of the breast cancer endocrine treatments
# over a loop for each period of interest
# and loops over age categories

# first filter out prostate treatments:
IR.age <- inc_data_OsteoDx_in_breastTAM

# filter out the both and male denominators
IR.age <- IR.age %>% filter(  denominator_cohort_id == 2 |denominator_cohort_id == 5|denominator_cohort_id == 8|
                                denominator_cohort_id == 11|denominator_cohort_id == 14)

# CREATE A NEW COLUMN OF Age and Sex groups
IR.age <- IR.age %>% mutate(Age_sex = case_when(grepl("2", denominator_cohort_id) ~ "Female; 0-150",
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
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate("Treatment-Related Outcome" = case_when(grepl("Bisphosphonates", age_sex) ~ "Bisphosphonates",
                                                                                                        grepl("Osteopenia", age_sex) ~ "Osteopenia",
                                                                                                        grepl("Bone Fracture", age_sex) ~ "Bone Fracture",
                                                                                                        grepl("Denosumab", age_sex) ~ "Denosumab",
                                                                                                        grepl("Osteoporosis", age_sex) ~ "Osteoporosis"))



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
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% pivot_wider(names_from = period, values_from = estimate) %>% arrange(`Treatment-Related Outcome`, `Age Group`)
#drop pre-lockdown reference column and re-order periods
#rate_ratios_table_formatted <- rate_ratios_table_formatted[c(1,2,5,6,7,8,4,9)]


#### Save IRR
write.csv(rate_ratios_table_formatted, file=here::here("3_IRR", db.name, "OsteoDxBreast",  "Observed IRR_OsteoDX_BreastTAM.csv"))
save(rate_ratios_table_formatted, file=here::here("3_IRR", db.name, "OsteoDxBreast",  "Observed IRR_OsteoDX_BreastTAM.RData"))

#### Make pretty table for breast cancer

Pretty_IRR_table_OsteoDXBreastTAM<- flextable(rate_ratios_table_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of treatment-related outcomes in breast cancer patients on tamoxifen over the lockdown periods compared to pre-COVID period, stratified by sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_OsteoDXBreastTAM' = Pretty_IRR_table_OsteoDXBreastTAM, path=here("3_IRR", db.name, "OsteoDxBreast",  "Pretty_IRR_table_OsteoDXBreastTAM.docx"))




# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

rateratios_table <- bind_rows(rateratios, .id = "age_sex")


# LAYOUT THE TABLE HOW YOU WANT IT
IRR_FOREST <- rateratios_table %>%  mutate_if(is.numeric, round, digits=2)


# CREATE A NEW COLUMN OF CANCER TYPES
IRR_FOREST  <- IRR_FOREST  %>% mutate("Treatment-Related Outcome" = case_when(grepl("Bisphosphonates", age_sex) ~ "Bisphosphonates",
                                                                                                              grepl("Osteopenia", age_sex) ~ "Osteopenia",
                                                                                                              grepl("Bone Fracture", age_sex) ~ "Bone Fracture",
                                                                                                              grepl("Denosumab", age_sex) ~ "Denosumab",
                                                                                                              grepl("Osteoporosis", age_sex) ~ "Osteoporosis"))


# CREATE A NEW COLUMN OF Age groups - THIS BIT OF CODE MERGES 80-150 AND 0-150 BECAUSE IT READS 80-150 WHEN IT READS 0-150
IRR_FOREST  <- IRR_FOREST  %>% mutate("Age Group" = case_when(grepl(" 0-150", age_sex) ~ "0-150",
                                                                                              grepl("20-39", age_sex) ~ "20-39",
                                                                                              grepl("40-59", age_sex) ~ "40-59",
                                                                                              grepl("60-79", age_sex) ~ "60-79",
                                                                                              grepl("80-150", age_sex) ~ "80-150"))

IRR_FOREST  <- IRR_FOREST  %>% mutate(period = recode(period, "Post-lockdown1" = "Post-first lockdown 1"))


# filter out pre-covid 
IRR_FOREST <- IRR_FOREST %>% filter(period !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST <- IRR_FOREST %>% rename("Lockdown Periods" = period)

# FILTER ONLY AGE 0-150
IRR_FOREST <- IRR_FOREST %>% filter(`Age Group` =="0-150")

IRR_FOREST <- IRR_FOREST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown 1", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_forest_osteoDx_Breast_TAM =
  ggplot(data=IRR_FOREST, aes(x = `Lockdown Periods`,y = estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Treatment-Related Outcome')+ ylab("Incidence Rate Ratio (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
  facet_wrap(~`Treatment-Related Outcome`,strip.position="left",nrow=4,scales = "free_y") +
  theme(plot.title=element_text(size=14,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold", size=12))+
  #panel.background = element_blank(),
  #panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"))+
  #guides(color=guide_legend(title="Lockdown Periods"), shape=guide_legend(title="Lockdown Periods"))+
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE))+
  # scale_fill_manual(values=cbPalette)+
  #scale_colour_manual(values=cbPalette)+
  coord_flip()+
  ggtitle("Incidence Rate Ratios of Endocrine Treatment-Related Outcomes in Breast Cancer Patients on Tamoxifen Across Lockdown Periods")+
  theme(plot.title = element_text(size = 11))

  
IRR_forest_osteoDx_Breast_TAM

# Save

ggsave(here("3_IRR", db.name, "OsteoDxBreast",  "IRR_OsteoDX_BreastTAM_forest.jpg"), IRR_forest_osteoDx_Breast_TAM, dpi=600, scale = 1,  width = 12, height = 10)


