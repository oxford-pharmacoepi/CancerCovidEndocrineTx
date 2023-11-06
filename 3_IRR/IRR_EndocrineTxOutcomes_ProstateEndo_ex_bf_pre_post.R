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


# add post-lockdown period which combines all periods from lockdown onwards
IR <- mutate(IR, covid2 =ifelse(covid == "Pre-COVID", "Pre-COVID", "All lockdown periods"))
pre_post_periods <- IR%>% dplyr::select("covid2")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()

sums_events_pre_post <- IR %>% group_by(outcome, covid2) %>% 
  summarise(sum_events=sum(events),
            sum_months=sum(months),
            .groups = 'drop')


# Rate ratios for bisphosphonates
Bis_sums <- sums_events_pre_post %>% filter(outcome =="Bisphosphonates")
Bis_sums <- Bis_sums[c(2,1),] # re-orders the rows so pre-covid first
Bis_sums <- Bis_sums %>% select(c(sum_events, sum_months))

rateratios_Bisphosphonates <- rateratio(as.matrix(Bis_sums, y=NULL))


################################################################################

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(pre_post_periods, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(pre_post_periods)
  names(get_IR_df_t) <- get_IR_df_t[1,]
  get_IR_df_t <- get_IR_df_t[-1,]
  rownames(get_IR_df_t) <- paste(title)
  return(get_IR_df_t)
}
# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS

IRR_Bisphosphonates <- get_IR_df_function(rateratios_Bisphosphonates, "Bisphosphonates") 


# JOIN THE TABLES
IRR_table_endodx_prostate <- IRR_Bisphosphonates
# REMOVE PRE-covid COLUMN
IRR_table_endodx_prostate <- IRR_table_endodx_prostate[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_endodx_prostate <- tibble::rownames_to_column(IRR_table_endodx_prostate, "Endocrine Treatment")

IRR_table_endodx_prostate_ex_bf_pre_post <- IRR_table_endodx_prostate
#### Save IRR
write.csv(IRR_table_endodx_prostate_ex_bf_pre_post, file=here::here(output.folder6, "IRR_table_endodx_prostate_ex_bf_pre_post.csv"))
save(IRR_table_endodx_prostate_ex_bf_pre_post, file=here::here(output.folder6, "IRR_table_endodx_prostate_ex_bf_pre_post.Rdata"))



# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON MONTHS FROM ALL OF THE LISTS FOR BISPHOSPHONATES

get_n_events_pm_function_bis <- function(yourrateratiosname, title){
  
  neventspm <- as.data.frame(yourrateratiosname[[1]])
  neventspm <- neventspm %>%  mutate_if(is.numeric, round, digits=2)
  
  # remove last row of totals
  neventspm <- neventspm[-3,]
  # add a column to indicate the covid period
  neventspm <- cbind(pre_post_periods, neventspm)
  
  # add column names
  names(neventspm)[1] <- "Periods"
  names(neventspm)[2] <- "N events"
  names(neventspm)[3] <- "Person MONTHS"
  
  # combine person MONTHS with n events
  neventspm <- neventspm %>% mutate(`n events / person MONTHS` = paste0(paste(`N events`)," (", paste(`Person MONTHS`), ")")) 
  
  # remove superfluous columns of events and person MONTHS
  neventspm <- neventspm[-c(2,3)]
  
  # transpose the table to have column headings as covid periods
  neventspm_t <- transpose(neventspm)
  #redefine row and column names
  colnames(neventspm_t) <- colnames(pre_post_periods)
  names(neventspm_t) <- neventspm_t[1,]
  neventspm_t <- neventspm_t[-1,]
  rownames(neventspm_t) <- paste(title)
  return(neventspm_t)
}


# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS

N_EVENTS_PM_Bisphosphonates <- get_n_events_pm_function_bis(rateratios_Bisphosphonates, "Bisphosphonates") 



# JOIN THE TABLES
N_EVENTS_PM_table_endodx_prostate_pre_post <- N_EVENTS_PM_Bisphosphonates
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PM_table_endodx_prostate_pre_post <- tibble::rownames_to_column(N_EVENTS_PM_table_endodx_prostate_pre_post, "Endocrine Treatment-Related Outcome")


#### Save n EVENTS AND PERSON MONTHS
write.csv(N_EVENTS_PM_table_endodx_prostate_pre_post, file=here::here(output.folder6, "N_EVENTS_PD_table_endodx_prostate_pre_post.csv"))
save(N_EVENTS_PM_table_endodx_prostate_pre_post, file=here::here(output.folder6, "N_EVENTS_PM_table_endodx_prostate_pre_post.Rdata"))


# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 


get_IR_df_function_CIs_Sep_bis <- function(yourrateratiosname, title){
  
  `Endocrine Treatment` <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(`Endocrine Treatment`, pre_post_periods, IR_CIS)
  
  return(IR_CIS)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
IRR_Bisphosphonates_Sep <- get_IR_df_function_CIs_Sep_bis(rateratios_Bisphosphonates, "Bisphosphonates") 

# JOIN THE RATIO OUTPUTS   
IRR_FOREST_endodx_prostate <- IRR_Bisphosphonates_Sep



# filter out pre-covid 
IRR_FOREST_endodx_prostate_pre_post <- IRR_FOREST_endodx_prostate %>% filter(pre_post_periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_endodx_prostate_pre_post <- IRR_FOREST_endodx_prostate_pre_post %>% rename("Lockdown Periods" = pre_post_periods) 


save(IRR_FOREST_endodx_prostate_pre_post, file=here::here(output.folder6, "IRR_FOREST_endodx_prostate_pre_post.Rdata"))


# Change char to num
IRR_FOREST_endodx_prostate_pre_post = 
  IRR_FOREST_endodx_prostate_pre_post %>% mutate(
    estimate_num = as.numeric(estimate),
    lower_num = as.numeric(lower),
    upper_num = as.numeric(upper))

# Save this as a data object so that i can combine it into the forest plot of all lockdown periods
save(IRR_FOREST_endodx_prostate_pre_post, file=here(output.folder6, "IRR_FOREST_endodx_prostate_pre_post.RData"))


# Then combine with the data frame for all lockdown periods from other script.
load("~/GitHub/CancerCovidEndocrineTx/3_IRR/CPRDGold_202207_patched/OsteoDxProstate/IRR_FOREST_endodx_prostate.Rdata")

IRR_FOREST_endodx_prostate_COMBINED_POST <- rbind(IRR_FOREST_endodx_prostate, IRR_FOREST_endodx_prostate_pre_post)

IRR_FOREST_endodx_prostate_COMBINED_POST <- IRR_FOREST_endodx_prostate_COMBINED_POST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed", "All lockdown periods"))) )

# Save this as a data object so that i can combine it into the forest plot of all lockdown periods
save(IRR_FOREST_endodx_prostate_COMBINED_POST, file=here(output.folder6, "IRR_FOREST_endodx_prostate_COMBINED_POST.RData"))


IRR_FOREST_endodx_prostate_plot_post =
  ggplot(IRR_FOREST_endodx_prostate_COMBINED_POST, aes(x = `Lockdown Periods`,y = estimate_num, ymin = lower_num, ymax = upper_num ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  xlab('Bisphosphonate Prescriptions in Prostate Cancer Patients on Endocrine Treatments')+ ylab("Incidence Rate Ratio on Logarithmic Scale (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  scale_shape_manual(values=c(0, 8, 9, 19, 15, 16, 17))+
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

IRR_FOREST_endodx_prostate_plot_post

# Save

ggsave(here(output.folder6, "IRR_FOREST_endodx_prostate_plot_post.tiff"), IRR_FOREST_endodx_prostate_plot_post, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here(output.folder6, "IRR_FOREST_endodx_prostate_plot_post.png"), IRR_FOREST_endodx_prostate_plot_post, dpi=600, scale = 1.3,  width = 10, height = 8)





