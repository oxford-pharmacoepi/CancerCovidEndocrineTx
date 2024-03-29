# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#         for treatment related outcomes in prostate cancer patients           #
#                         on any endocrine treatment                           #
#                              Nicola Barclay                                  #
#                                18-08-2023                                    #
# ============================================================================ #

library(epitools)
library(data.table)

# ===================== Prostate CANCER ======================================== #

# Load the cleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

inc_data <- read_csv("0_DataPrep/inc_data_endo_dx_outcomes_in_prostate.csv")


# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# ============ OVERALL (NO AGE SEX STRATIFICATION ) ========================== #


# This code calculates the IRR for each of the cancers
# separately, but loops over each period of interest

IR.overall <- inc_data %>% filter(denominator_cohort_id ==1)

IR <- IR.overall

periods_bis<- IR%>% filter(outcome == "Bisphosphonates") %>% dplyr::select("covid")%>%distinct()%>%pull()

outcome_bis <-IR%>% filter(outcome == "Bisphosphonates") %>% dplyr::select("outcome")%>% distinct()%>%pull()

# EXTRACT THE  LISTS

# for bisphosphonates

rateratios <- vector("list",length(outcome_bis)); names(rateratios) = outcome_bis

for (y in 1:length(outcome_bis)){
  working.outcome <- outcome_bis[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods_bis)){ 
    working.period <- periods_bis[z]
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter(covid==working.period) %>%
      mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
      group_by(ref)%>% #no function for final time period
      summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
      mutate(periods = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
    
    vector <- rbind(vector,c(events, pt))
    
    
  }  
  ifelse(dim(vector)[1] > 1,
         rateratios[[y]] <-rateratio(as.matrix(vector, y=NULL)), # this bit of the code says that if there is a result in vector, give us the rate ratio. If no result, give us NA
         rateratios[[y]] <- NA)
} 


# EXTRACT THE  LISTS
rateratios_Bisphosphonates <- rateratios$`Bisphosphonates`


################################################################################


# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS FOR BISPHOSPHONATES

get_IR_df_function_bis <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(periods_bis, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(periods_bis)
  names(get_IR_df_t) <- get_IR_df_t[1,]
  get_IR_df_t <- get_IR_df_t[-1,]
  rownames(get_IR_df_t) <- paste(title)
  return(get_IR_df_t)
}


# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
IRR_Bisphosphonates <- get_IR_df_function_bis(rateratios_Bisphosphonates, "Bisphosphonates") 

# ADD COLUMNS FOR MISSING PERIODS, WITH NAS
IRR_Bisphosphonates$`Third lockdown` <- c(NA)
IRR_Bisphosphonates<-IRR_Bisphosphonates[c(1,2,3,4,7,5,6)]


# JOIN THE TABLES
IRR_table_endodx_prostate <- IRR_Bisphosphonates
# REMOVE PRE-covid COLUMN
IRR_table_endodx_prostate <- IRR_table_endodx_prostate[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_endodx_prostate <- tibble::rownames_to_column(IRR_table_endodx_prostate, "Endocrine Treatment")

IRR_table_endodx_prostate_ex_bf <- IRR_table_endodx_prostate
#### Save IRR
write.csv(IRR_table_endodx_prostate_ex_bf, file=here::here(output.folder6, "IRR_table_endodx_prostate_ex_bf.csv"))
save(IRR_table_endodx_prostate_ex_bf, file=here::here(output.folder6, "IRR_table_endodx_prostate_ex_bf.Rdata"))

#### Make pretty table
Pretty_IRR_table_endodx_prostate_ex_bf <- flextable(IRR_table_endodx_prostate_ex_bf) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of treatment-related outcomes in prostate cancer patients on endocrine treatments over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_endodx_prostate_ex_bf' = Pretty_IRR_table_endodx_prostate_ex_bf, path=here(output.folder6, "Pretty_IRR_table_endodx_prostate_ex_bf.docx"))



# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON MONTHS FROM ALL OF THE LISTS FOR BISPHOSPHONATES

get_n_events_pd_function_bis <- function(yourrateratiosname, title){
  
  neventspd <- as.data.frame(yourrateratiosname[[1]])
  neventspd <- neventspd %>%  mutate_if(is.numeric, round, digits=2)
  
  # remove last row of totals
  neventspd <- neventspd[-7,]
  # add a column to indicate the covid period
  neventspd <- cbind(periods_bis, neventspd)
  
  # add column names
  names(neventspd)[1] <- "Periods"
  names(neventspd)[2] <- "N events"
  names(neventspd)[3] <- "Person MONTHS"
  
  # combine person MONTHS with n events
  neventspd <- neventspd %>% mutate(`n events / person MONTHS` = paste0(paste(`N events`)," (", paste(`Person MONTHS`), ")")) 
  
  # remove superfluous columns of events and person MONTHS
  neventspd <- neventspd[-c(2,3)]
  
  # transpose the table to have column headings as covid periods
  neventspd_t <- transpose(neventspd)
  #redefine row and column names
  colnames(neventspd_t) <- colnames(periods_bis)
  names(neventspd_t) <- neventspd_t[1,]
  neventspd_t <- neventspd_t[-1,]
  rownames(neventspd_t) <- paste(title)
  return(neventspd_t)
}


# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS

N_EVENTS_PD_Bisphosphonates <- get_n_events_pd_function_bis(rateratios_Bisphosphonates, "Bisphosphonates") 

# ADD COLUMNS FOR MISSING PERIODS, WITH NAS
N_EVENTS_PD_Bisphosphonates$`Third lockdown` <- c(NA)
N_EVENTS_PD_Bisphosphonates<- N_EVENTS_PD_Bisphosphonates[c(1,2,3,4,7,5,6)]



# JOIN THE TABLES
N_EVENTS_PD_table_endodx_prostate <- N_EVENTS_PD_Bisphosphonates
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PD_table_endodx_prostate <- tibble::rownames_to_column(N_EVENTS_PD_table_endodx_prostate, "Endocrine Treatment-Related Outcome")



#### Save n EVENTS AND PERSON MONTHS
write.csv(N_EVENTS_PD_table_endodx_prostate, file=here::here(output.folder6, "N_EVENTS_PD_table_endodx_prostate.csv"))
save(N_EVENTS_PD_table_endodx_prostate, file=here::here(output.folder6, "N_EVENTS_PD_table_endodx_prostate.Rdata"))

#### Make pretty table
Pretty_N_EVENTS_PD_table_endodx_prostate <- flextable(N_EVENTS_PD_table_endodx_prostate) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of events and person MONTHS of treatment-related outcomes in prostate cancer patients on endocrine treatments over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_N_EVENTS_PD_table_endodx_prostate' = Pretty_N_EVENTS_PD_table_endodx_prostate, path=here(output.folder6, "Pretty_N_EVENTS_PD_table_endodx_prostate.docx"))


# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 


get_IR_df_function_CIs_Sep_bis <- function(yourrateratiosname, title){
  
  `Endocrine Treatment` <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(`Endocrine Treatment`, periods_bis, IR_CIS)
  
  return(IR_CIS)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
IRR_Bisphosphonates_Sep <- get_IR_df_function_CIs_Sep_bis(rateratios_Bisphosphonates, "Bisphosphonates") 

# ADD ROWS FOR MISSING PERIODS, WITH NAS
new_row = c(`Endocrine Treatment` = "Bisphosphonates", periods_bis="Third lockdown", estimate = NA, lower = NA, upper = NA)
IRR_Bisphosphonates_Sep <- rbind(IRR_Bisphosphonates_Sep,new_row)
IRR_Bisphosphonates_Sep<- IRR_Bisphosphonates_Sep[c(1,2,3,4,7,5,6),]
IRR_Bisphosphonates_Sep <- IRR_Bisphosphonates_Sep %>% rename("Periods" = periods_bis)

# JOIN THE RATIO OUTPUTS   
IRR_FOREST_endodx_prostate <- IRR_Bisphosphonates_Sep



# filter out pre-covid 
IRR_FOREST_endodx_prostate <- IRR_FOREST_endodx_prostate %>% filter(Periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_endodx_prostate <- IRR_FOREST_endodx_prostate %>% rename("Lockdown Periods" = Periods) 

# RENAME POST-LOCKDOWN 1
IRR_FOREST_endodx_prostate$`Lockdown Periods`[IRR_FOREST_endodx_prostate$`Lockdown Periods` == "Post-lockdown1"] <- "Post-first lockdown"

IRR_FOREST_endodx_prostate <- IRR_FOREST_endodx_prostate  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown", "Second lockdown", 
                                                                        "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

save(IRR_FOREST_endodx_prostate, file=here::here(output.folder6, "IRR_FOREST_endodx_prostate.Rdata"))

str(IRR_FOREST_endodx_prostate) 

# Change char to num
IRR_FOREST_endodx_prostate = 
  IRR_FOREST_endodx_prostate %>% mutate(
    estimate_num = as.numeric(estimate),
    lower_num = as.numeric(lower),
    upper_num = as.numeric(upper))


save(IRR_FOREST_endodx_prostate, file=here::here(output.folder6, "IRR_FOREST_endodx_prostate.Rdata"))

IRR_FOREST_endodx_prostate_plot =
  ggplot(IRR_FOREST_endodx_prostate, aes(x = `Lockdown Periods`,y = estimate_num, ymin = lower_num, ymax = upper_num ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  xlab('Bisphosphonate Prescriptions in Prostate Cancer Patients on Endocrine Treatments')+ ylab("Incidence Rate Ratio on Logarithmic Scale (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  scale_y_log10() +
  geom_errorbar(aes(ymin=lower_num, ymax=upper_num,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
  theme(plot.title=element_text(size=14,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold", size=12))+
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  coord_flip()

IRR_FOREST_endodx_prostate_plot

# Save

ggsave(here(output.folder6, "IRR_FOREST_endodx_prostate_plot_ex_bf.tiff"), IRR_FOREST_endodx_prostate_plot, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here(output.folder6, "IRR_FOREST_endodx_prostate_plot_ex_bf.png"), IRR_FOREST_endodx_prostate_plot, dpi=600, scale = 1.3,  width = 10, height = 8)



#### INCIDENCE RATES TABLES FOR PAPER --------------------------------------- ##

#This gives you all the rates calculated in each of the time periods
overall <-IR.overall%>% group_by(covid, outcome) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)

ir <- rbind(overall)%>% arrange(covid, outcome)

ir1 <-as.matrix(ir[,3:4])
ci <- round(epi.conf(ir1, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
                     conf.level = 0.95) * 100000,1)

ir_ci <- cbind(ir, ci)
ir_ci <- ir_ci %>% 
  mutate(ir = paste0(paste(est),"(", paste(lower), " to ", paste(upper), ")"))%>%
  dplyr::select(covid, outcome, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)



# add combined periods post-lockdown - this gives you all the IR calculated anytime after lockdown.These are not averaged but caluclated
overall.post <-IR.overall%>% 
  filter(months.since.start >=39)%>%
  group_by(outcome) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)

ir_post <- bind_rows(overall.post)%>% arrange(outcome)
ir2 <-as.matrix(ir_post[,2:3])
ci2 <- round(epi.conf(ir2, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
                      conf.level = 0.95) * 100000,1)

ir_ci2 <- cbind(ir_post, ci2)
ir.post_ci <- ir_ci2 %>% 
  mutate(ir = paste0(paste(est)," (", paste(lower), " to ", paste(upper), ")"))%>%
  mutate(covid="Post-lockdown")%>%
  dplyr::select(covid,outcome,  events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)




# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci, ir.post_ci)


# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 
# re-order columns
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(1, 6,4,5,7,8,2,3,9)]
#ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(2,4,8,10,12,13,3,6,14,5,7,9,1,11), c(1, 2, 5, 9, 6, 7, 8, 3,4)]
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename("Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (March 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-first lockdown 1 (July 2020-Oct 2020)" = "Post-lockdown1",
                                                        "Second lockdown (Nov 2020-Dec 2020)" = "Second lockdown", 
                                                        "Third lockdown (Jan 2021-Feb 2021)" = "Third lockdown",
                                                        "Easing of restrictions (March 2021-June 2021" = "Easing of restrictions", 
                                                        "Legal restrictions removed (July 2021-Dec 2021)"= "Legal restrictions removed")


Pretty_observed_IR_results_endodx_prostate <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of endocrine-treatment related outcomes in prostate cancer patients on any endocrine treatment in each of the time periods") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here(output.folder6, "IR_table_endodx_prostate_with_pre_post_lockdown_pivot.RData"))
write.csv(ir_ci_pre_post_pivot, file=here(output.folder6, "IR_table_endodx_prostate_with_pre_post_lockdown_pivot.csv"))

save_as_docx('Pretty_observed_IR_results_endodx_prostate' = Pretty_observed_IR_results_endodx_prostate, path=here(output.folder6, "Pretty_observed_IR_results_endodx_prostate.docx"))





