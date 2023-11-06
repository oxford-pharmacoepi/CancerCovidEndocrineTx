# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#         for treatment related outcomes in breast cancer patients             #
#                         on Aromatase Inhibitors                              #
#                              Nicola Barclay                                  #
#                                18-08-2023                                    #
# ============================================================================ #

library(epitools)
library(data.table)
library(readr)

# ===================== BREAST CANCER ======================================== #

# Load the cleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

inc_data <- read_csv("0_DataPrep/inc_data_endo_dx_outcomes_in_breast_AI.csv")


# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# ============ OVERALL (NO AGE SEX STRATIFICATION ) ========================== #


# This code calculates the IRR for each of the cancers
# separately, but loops over each period of interest

IR.overall <- inc_data %>% filter(denominator_cohort_id ==2)# this is females only

IR <- IR.overall

# add post-lockdown period which combines all periods from lockdown onwards
IR <- mutate(IR, covid2 =ifelse(covid == "Pre-COVID", "Pre-COVID", "All lockdown periods"))
pre_post_periods <- IR%>% dplyr::select("covid2")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()

sums_events_pre_post <- IR %>% group_by(outcome, covid2) %>% 
  summarise(sum_events=sum(events),
            sum_months=sum(months),
            .groups = 'drop')

# Rate ratios for osteopenia
osteopenia_sums <- sums_events_pre_post %>% filter(outcome =="Osteopenia")
osteopenia_sums <- osteopenia_sums[c(2,1),] # re-orders the rows so pre-covid first
osteopenia_sums <- osteopenia_sums %>% select(c(sum_events, sum_months))

rateratios_Osteopenia <- rateratio(as.matrix(osteopenia_sums, y=NULL))

# Rate ratios for osteoporosis
Osteoporosis_sums <- sums_events_pre_post %>% filter(outcome =="Osteoporosis")
Osteoporosis_sums <- Osteoporosis_sums[c(2,1),] # re-orders the rows so pre-covid first
Osteoporosis_sums <- Osteoporosis_sums %>% select(c(sum_events, sum_months))

rateratios_Osteoporosis <- rateratio(as.matrix(Osteoporosis_sums, y=NULL))

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

IRR_Osteopenia  <-  get_IR_df_function(rateratios_Osteopenia, "Osteopenia")
IRR_Osteoporosis <-  get_IR_df_function(rateratios_Osteoporosis, "Osteoporosis")
IRR_Bisphosphonates <- get_IR_df_function(rateratios_Bisphosphonates, "Bisphosphonates") 

# JOIN THE TABLES
IRR_table_endodx_breastAI_POST <- rbind(IRR_Osteopenia, IRR_Osteoporosis, IRR_Bisphosphonates)
# REMOVE PRE-covid COLUMN
IRR_table_endodx_breastAI_POST <- IRR_table_endodx_breastAI_POST[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_endodx_breastAI_POST <- tibble::rownames_to_column(IRR_table_endodx_breastAI_POST, "Endocrine Treatment")
IRR_table_endodx_breastAI_POST <- IRR_table_endodx_breastAI_POST[c(3,4,1,2),]

#### Save IRR
write.csv(IRR_table_endodx_breastAI_POST, file=here::here(output.folder5, "IRR_table_endodx_breastAI_POST.csv"))
save(IRR_table_endodx_breastAI_POST, file=here::here(output.folder5, "IRR_table_endodx_breastAI_POST.Rdata"))



# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON MONTHS FROM ALL OF THE LISTS 

get_n_events_pm_function <- function(yourrateratiosname, title){
  
  neventspm <- as.data.frame(yourrateratiosname[[1]])
  neventspm <- neventspm %>%  mutate_if(is.numeric, round, digits=2)
  
  # remove last row of totals
  neventspm <- neventspm[-3,]
  # add a column to indicate the covid period
  neventspm <- cbind(pre_post_periods, neventspm)
  
  # add column names
  names(neventspm)[1] <- "Periods"
  names(neventspm)[2] <- "N events"
  names(neventspm)[3] <- "Person Months"
  
  # combine person months with n events
  neventspm <- neventspm %>% mutate(`n events / person months` = paste0(paste(`N events`)," (", paste(`Person Months`), ")")) 
  
  # remove superfluous columns of events and person months
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

N_EVENTS_PM_Osteopenia  <-  get_n_events_pm_function(rateratios_Osteopenia, "Osteopenia")
N_EVENTS_PM_Osteoporosis <-  get_n_events_pm_function(rateratios_Osteoporosis, "Osteoporosis")
N_EVENTS_PM_Bisphosphonates <- get_n_events_pm_function(rateratios_Bisphosphonates, "Bisphosphonates") 


# JOIN THE TABLES
N_EVENTS_PM_table_endodx_breastAI <- rbind(N_EVENTS_PM_Osteopenia, N_EVENTS_PM_Osteoporosis, N_EVENTS_PM_Bisphosphonates)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PM_table_endodx_breastAI <- tibble::rownames_to_column(N_EVENTS_PM_table_endodx_breastAI, "Endocrine Treatment-Related Outcome")
# re-order the rows
N_EVENTS_PM_table_endodx_breastAI <- N_EVENTS_PM_table_endodx_breastAI[c(3,4,1,2),]

#### Save n EVENTS AND PERSON MONTHS
write.csv(N_EVENTS_PM_table_endodx_breastAI, file=here::here(output.folder5, "N_EVENTS_PM_table_endodx_breastAI.csv"))
save(N_EVENTS_PM_table_endodx_breastAI, file=here::here(output.folder5, "N_EVENTS_PM_table_endodx_breastAI.Rdata"))



# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function_CIs_Sep <- function(yourrateratiosname, title){
  
  `Endocrine Treatment` <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(`Endocrine Treatment`, pre_post_periods, IR_CIS)
  
  return(IR_CIS)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
IRR_Osteopenia_Sep  <-  get_IR_df_function_CIs_Sep(rateratios_Osteopenia, "Osteopenia")
IRR_Osteoporosis_Sep <-  get_IR_df_function_CIs_Sep(rateratios_Osteoporosis, "Osteoporosis")
IRR_Bisphosphonates_Sep <- get_IR_df_function_CIs_Sep(rateratios_Bisphosphonates, "Bisphosphonates") 


# JOIN THE RATIO OUTPUTS   
IRR_FOREST_endodx_breastAI_POST <- rbind(IRR_Osteopenia_Sep, IRR_Osteoporosis_Sep, IRR_Bisphosphonates_Sep)

# filter out pre-covid 
IRR_FOREST_endodx_breastAI_POST <- IRR_FOREST_endodx_breastAI_POST %>% filter(pre_post_periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_endodx_breastAI_POST <- IRR_FOREST_endodx_breastAI_POST %>% rename("Lockdown Periods" = pre_post_periods) 




# Change char to num
IRR_FOREST_endodx_breastAI_POST = 
  IRR_FOREST_endodx_breastAI_POST %>% mutate(
    estimate_num = as.numeric(estimate),
    lower_num = as.numeric(lower),
    upper_num = as.numeric(upper))

# Save this as a data object so that i can combine it into the forest plot of all lockdown periods
save(IRR_FOREST_endodx_breastAI_POST, file=here(output.folder3, "IRR_FOREST_endodx_breastAI_POST.RData"))

# Then combine with the data frame for all lockdown periods from other script.
load("~/GitHub/CancerCovidEndocrineTx/3_IRR/CPRDGold_202207_patched/OsteoDxBreast/IRR_FOREST_endodx_breastAI.RData")

IRR_FOREST_endodx_breastAI_COMBINED_POST <- rbind(IRR_FOREST_endodx_breastAI, IRR_FOREST_endodx_breastAI_POST)

IRR_FOREST_endodx_breastAI_COMBINED_POST <- IRR_FOREST_endodx_breastAI_COMBINED_POST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed", "All lockdown periods"))) )




# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_endodx_breastAI_plot_POST =
  ggplot(data=IRR_FOREST_endodx_breastAI_COMBINED_POST, aes(x = `Lockdown Periods`,y = estimate_num, ymin = lower_num, ymax = upper_num ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  scale_shape_manual(values=c(0, 8, 9, 19, 15, 16, 17))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Endocrine Treatment')+ ylab("Incidence Rate Ratio on Logarithmic Scale (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_errorbar(aes(ymin=lower_num, ymax=upper_num,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
 scale_y_log10() +
  facet_wrap(~`Endocrine Treatment`,strip.position="left",nrow=4,scales = "free_y",labeller = label_wrap_gen()) +
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
  coord_flip()


IRR_FOREST_endodx_breastAI_plot_POST


# Save

ggsave(here(output.folder5, "IRR_FOREST_endodx_breastAI_plot_POST.tiff"), IRR_FOREST_endodx_breastAI_plot_POST, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here(output.folder5, "IRR_FOREST_endodx_breastAI_plot_POST.jpg"), IRR_FOREST_endodx_breastAI_plot_POST, dpi=600, scale = 1.3,  width = 10, height = 8)


