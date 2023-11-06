# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#         for endocrine treatments in breast and prostate cancer patients      #
#                                                                              #
#                              Nicola Barclay                                  #
#                                18-08-2023                                    #
# ============================================================================ #

library(epitools)
library(data.table)
library(epiR)
# ===================== BREAST CANCER ======================================== #

# Load the cleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

inc_data <- read_csv("0_DataPrep/inc_data_endo_tx_in_breast.csv")


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

# Rate ratios for AIs
AI_sums <- sums_events_pre_post %>% filter(outcome =="Aromatase Inhibitors")
AI_sums <- AI_sums[c(2,1),] # re-orders the rows so pre-covid first
AI_sums <- AI_sums %>% select(c(sum_events, sum_months))

rateratios_AromataseInhibitors <- rateratio(as.matrix(AI_sums, y=NULL))

# Rate ratios for AIs with gnrh
AI_GnRH_sums <- sums_events_pre_post %>% filter(outcome =="Aromatase Inhibitors with GnRH Agonists Or Antagonists")
AI_GnRH_sums <- AI_GnRH_sums[c(2,1),] # re-orders the rows so pre-covid first
AI_GnRH_sums <- AI_GnRH_sums %>% select(c(sum_events, sum_months))

rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists <- rateratio(as.matrix(AI_GnRH_sums, y=NULL))

# Rate ratios for Tamoxifen with gnrh
TAM_GnRH_sums <- sums_events_pre_post %>% filter(outcome =="Tamoxifen with GnRH Agonists Or Antagonists")
TAM_GnRH_sums <- TAM_GnRH_sums[c(2,1),] # re-orders the rows so pre-covid first
TAM_GnRH_sums <- TAM_GnRH_sums %>% select(c(sum_events, sum_months))

rateratios_TamoxifenGnRHAgonistsAntagonists <- rateratio(as.matrix(TAM_GnRH_sums, y=NULL))

# Rate ratios for Tamoxifen 
TAM_sums <- sums_events_pre_post %>% filter(outcome =="Tamoxifen")
TAM_sums <- TAM_sums[c(2,1),] # re-orders the rows so pre-covid first
TAM_sums <- TAM_sums %>% select(c(sum_events, sum_months))

rateratios_Tamoxifen <- rateratio(as.matrix(TAM_sums, y=NULL))



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

IRR_AIs_with_GNRH <-  get_IR_df_function(rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists, "Aromatase Inhibitors with GnRH Agonists Or Antagonists")
IRR_AIs <-  get_IR_df_function(rateratios_AromataseInhibitors, "Aromatase Inhibitors")
IRR_Tamoxifen_with_GNRH <- get_IR_df_function(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
IRR_Tamoxifen <-  get_IR_df_function(rateratios_Tamoxifen, "Tamoxifen")


# JOIN THE TABLES
IRR_table_breast_endo_post <- rbind(IRR_AIs_with_GNRH, IRR_AIs, IRR_Tamoxifen_with_GNRH, IRR_Tamoxifen)
# REMOVE PRE-covid COLUMN
IRR_table_breast_endo_post <- IRR_table_breast_endo_post[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_breast_endo_post <- tibble::rownames_to_column(IRR_table_breast_endo_post, "Endocrine Treatment")


#### Save IRR
write.csv(IRR_table_breast_endo_post, file=here::here(output.folder3, "IRR_table_breast_endo_post.csv"))
save(IRR_table_breast_endo_post, file=here::here(output.folder3, "IRR_table_breast_endo_post.Rdata"))




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
N_EVENTS_PM_AIs_with_GNRH <-  get_n_events_pm_function(rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists, "Aromatase Inhibitors with GnRH Agonists Or Antagonists")
N_EVENTS_PM_AIs <-  get_n_events_pm_function(rateratios_AromataseInhibitors, "Aromatase Inhibitors")
N_EVENTS_PM_Tamoxifen_with_GNRH <- get_n_events_pm_function(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
N_EVENTS_PM_Tamoxifen <-  get_n_events_pm_function(rateratios_Tamoxifen, "Tamoxifen")



# JOIN THE TABLES
N_EVENTS_PM_table_endoTx_breast <- rbind(N_EVENTS_PM_AIs, N_EVENTS_PM_AIs_with_GNRH, N_EVENTS_PM_Tamoxifen, N_EVENTS_PM_Tamoxifen_with_GNRH)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PM_table_endoTx_breast <- tibble::rownames_to_column(N_EVENTS_PM_table_endoTx_breast, "Endocrine Treatment-Related Outcome")


#### Save n EVENTS AND PERSON MONTHS
write.csv(N_EVENTS_PM_table_endoTx_breast, file=here::here(output.folder3, "N_EVENTS_PM_table_endoTx_breast_POST.csv"))
save(N_EVENTS_PM_table_endoTx_breast, file=here::here(output.folder3, "N_EVENTS_PM_table_endoTx_breast_POST.Rdata"))

#### Make pretty table
Pretty_N_EVENTS_PD_table_endoTx_breast_POST <- flextable(N_EVENTS_PD_table_endoTx_breast) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of events and person MONTHS of endocrine treatments in breast cancer patients POST lockdown compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_N_EVENTS_PD_table_endoTx_breast_POST' = Pretty_N_EVENTS_PD_table_endoTx_breast_POST, path=here(output.folder3, "Pretty_N_EVENTS_PD_table_endoTx_breast_POST.docx"))





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
IRR_AIs_with_GNRH_Sep <-  get_IR_df_function_CIs_Sep(rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists, "Aromatase Inhibitors with GnRH Agonists Or Antagonists")
IRR_AIs_Sep <-  get_IR_df_function_CIs_Sep(rateratios_AromataseInhibitors, "Aromatase Inhibitors")
IRR_Tamoxifen_with_GNRH_Sep <- get_IR_df_function_CIs_Sep(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
IRR_Tamoxifen_Sep <-  get_IR_df_function_CIs_Sep(rateratios_Tamoxifen, "Tamoxifen")


# JOIN THE RATIO OUTPUTS   
IRR_FOREST_ENDO_BREAST_POST <- rbind(IRR_AIs_with_GNRH_Sep, IRR_AIs_Sep, IRR_Tamoxifen_with_GNRH_Sep,  IRR_Tamoxifen_Sep)

# filter out pre-covid 
IRR_FOREST_ENDO_BREAST_POST <- IRR_FOREST_ENDO_BREAST_POST %>% filter(pre_post_periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_ENDO_BREAST_POST <- IRR_FOREST_ENDO_BREAST_POST %>% rename("Lockdown Periods" = pre_post_periods) 

# Change char to num
IRR_FOREST_ENDO_BREAST_POST = 
  IRR_FOREST_ENDO_BREAST_POST %>% mutate(
    estimate_num = as.numeric(estimate),
    lower_num = as.numeric(lower),
    upper_num = as.numeric(upper))

# Save this as a data object so that i can combine it into the forest plot of all lockdown periods
save(IRR_FOREST_ENDO_BREAST_POST, file=here(output.folder3, "IRR_FOREST_ENDO_BREAST_POST.RData"))

# Then combine with the data frame for all lockdown periods from other script.
load("~/GitHub/CancerCovidEndocrineTx/3_IRR/CPRDGold_202207_patched/EndoTxBreast/IRR_FOREST_ENDO_BREAST_COMBINED.RData")


IRR_FOREST_ENDO_BREAST_COMBINED_POST <- rbind(IRR_FOREST_ENDO_BREAST_COMBINED, IRR_FOREST_ENDO_BREAST_POST)

IRR_FOREST_ENDO_BREAST_COMBINED_POST <- IRR_FOREST_ENDO_BREAST_COMBINED_POST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed", "All lockdown periods"))) )



# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_ENDO_BREAST_PLOT_POST =
  ggplot(data=IRR_FOREST_ENDO_BREAST_COMBINED_POST, aes(x = `Lockdown Periods`,y = estimate_num, ymin = lower_num, ymax = upper_num ))+
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


IRR_FOREST_ENDO_BREAST_PLOT_POST

# Save

ggsave(here(output.folder3, "IRR_FOREST_ENDO_BREAST_PLOT_POST.tiff"), IRR_FOREST_ENDO_BREAST_PLOT_POST, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here(output.folder3, "IRR_FOREST_ENDO_BREAST_PLOT_POST.jpg"), IRR_FOREST_ENDO_BREAST_PLOT_POST, dpi=600, scale = 1.3,  width = 10, height = 8)



################################################################################

# ===================== PROSTATE CANCER ======================================== #

# Load the cleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

inc_data <- read_csv("0_DataPrep/inc_data_endo_tx_in_prostate.csv")


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

# Rate ratios for 1st generation antiandrogens
First_gen_sums <- sums_events_pre_post %>% filter(outcome =="First generation antiandrogens")
First_gen_sums <- First_gen_sums[c(2,1),] # re-orders the rows so pre-covid first
First_gen_sums <- First_gen_sums %>% select(c(sum_events, sum_months))

rateratios_First_gen_antiandrogens <- rateratio(as.matrix(First_gen_sums, y=NULL))

# Rate ratios for 1st generation antiandrogens with gnrh
First_gen_gnrh_sums <- sums_events_pre_post %>% filter(outcome =="GNRH Agonists with 1st Generation ADT")
First_gen_gnrh_sums <- First_gen_gnrh_sums[c(2,1),] # re-orders the rows so pre-covid first
First_gen_gnrh_sums <- First_gen_gnrh_sums %>% select(c(sum_events, sum_months))

rateratios_GNRH_Agonists_with_1st_Gen_ADT <- rateratio(as.matrix(First_gen_gnrh_sums, y=NULL))

# Rate ratios for gnrh agonists
GnRH_agonists_sums <- sums_events_pre_post %>% filter(outcome =="GNRH Agonists")
GnRH_agonists_sums <- GnRH_agonists_sums[c(2,1),] # re-orders the rows so pre-covid first
GnRH_agonists_sums <- GnRH_agonists_sums %>% select(c(sum_events, sum_months))

rateratios_GNRH_Agonists <- rateratio(as.matrix(GnRH_agonists_sums, y=NULL))

# Rate ratios for GnRH LHRH antagonists 
GNRH_LHRHantagonists_sums <- sums_events_pre_post %>% filter(outcome =="GNRH / LHRH antagonists")
GNRH_LHRHantagonists_sums <- GNRH_LHRHantagonists_sums[c(2,1),] # re-orders the rows so pre-covid first
GNRH_LHRHantagonists_sums <- GNRH_LHRHantagonists_sums %>% select(c(sum_events, sum_months))

rateratios_GNRH_LHRHantagonists <- rateratio(as.matrix(GNRH_LHRHantagonists_sums, y=NULL))




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

IRR_First_gen_antiandrogens <-  get_IR_df_function(rateratios_First_gen_antiandrogens, "First generation antiandrogens")
IRR_GNRH_Agonists_with_1st_Gen_ADT <-  get_IR_df_function(rateratios_GNRH_Agonists_with_1st_Gen_ADT, "GNRH Agonists with 1st Generation ADT")
IRR_GNRH_Agonists <- get_IR_df_function(rateratios_GNRH_Agonists, "GNRH Agonists") 
IRR_GNRH_LHRHantagonists <-  get_IR_df_function(rateratios_GNRH_LHRHantagonists, "GNRH / LHRH antagonists")


# JOIN THE TABLES
IRR_table_prostate_endo_post <- rbind(IRR_First_gen_antiandrogens, IRR_GNRH_Agonists, IRR_GNRH_Agonists_with_1st_Gen_ADT, IRR_GNRH_LHRHantagonists)
# REMOVE PRE-covid COLUMN
IRR_table_prostate_endo_post <- IRR_table_prostate_endo_post[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_prostate_endo_post <- tibble::rownames_to_column(IRR_table_prostate_endo_post, "Endocrine Treatment")

#### Save IRR
write.csv(IRR_table_prostate_endo_post, file=here::here(output.folder4, "IRR_table_prostate_endo_post.csv"))
save(IRR_table_prostate_endo_post, file=here::here(output.folder4, "IRR_table_prostate_endo_post.Rdata"))



# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS

N_EVENTS_PM_First_gen_antiandrogens <-  get_n_events_pm_function(rateratios_First_gen_antiandrogens, "First generation antiandrogens")
N_EVENTS_PM_GNRH_Agonists_with_1st_Gen_ADT <-  get_n_events_pm_function(rateratios_GNRH_Agonists_with_1st_Gen_ADT, "GNRH Agonists with 1st Generation ADT")
N_EVENTS_PM_GNRH_Agonists <- get_n_events_pm_function(rateratios_GNRH_Agonists, "GNRH Agonists") 
N_EVENTS_PM_GNRH_LHRHantagonists <-  get_n_events_pm_function(rateratios_GNRH_LHRHantagonists, "GNRH / LHRH antagonists")


# JOIN THE TABLES
N_EVENTS_PM_table_endo_prostate_post <- rbind(N_EVENTS_PM_First_gen_antiandrogens, N_EVENTS_PM_GNRH_Agonists, N_EVENTS_PM_GNRH_Agonists_with_1st_Gen_ADT, N_EVENTS_PM_GNRH_LHRHantagonists)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PM_table_endo_prostate_post <- tibble::rownames_to_column(N_EVENTS_PM_table_endo_prostate_post, "Endocrine Treatment")


#### Save n EVENTS AND PERSON MONTHS
write.csv(N_EVENTS_PM_table_endo_prostate_post, file=here::here(output.folder4, "N_EVENTS_PM_table_endo_prostate_post.csv"))
save(N_EVENTS_PM_table_endo_prostate_post, file=here::here(output.folder4, "N_EVENTS_PM_table_endo_prostate_post.Rdata"))




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
IRR_First_gen_antiandrogens_Sep <-  get_IR_df_function_CIs_Sep(rateratios_First_gen_antiandrogens, "First generation antiandrogens")
IRR_GNRH_Agonists_with_1st_Gen_ADT_Sep <-  get_IR_df_function_CIs_Sep(rateratios_GNRH_Agonists_with_1st_Gen_ADT, "GNRH Agonists with 1st Generation ADT")
IRR_GNRH_Agonists_Sep <- get_IR_df_function_CIs_Sep(rateratios_GNRH_Agonists, "GNRH Agonists") 
IRR_GNRH_LHRHantagonists_Sep <-  get_IR_df_function_CIs_Sep(rateratios_GNRH_LHRHantagonists, "GNRH / LHRH antagonists")



# JOIN THE RATIO OUTPUTS   
IRR_FOREST_ENDO_prostate_post <- rbind(IRR_First_gen_antiandrogens_Sep, IRR_GNRH_Agonists_Sep, IRR_GNRH_Agonists_with_1st_Gen_ADT_Sep, IRR_GNRH_LHRHantagonists_Sep)

# filter out pre-covid 
IRR_FOREST_ENDO_prostate_post <- IRR_FOREST_ENDO_prostate_post %>% filter(pre_post_periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_ENDO_prostate_post <- IRR_FOREST_ENDO_prostate_post %>% rename("Lockdown Periods" = pre_post_periods) 

                                      
IRR_FOREST_ENDO_prostate_post <- IRR_FOREST_ENDO_prostate_post  %>%
  mutate(`Endocrine Treatment` = factor(`Endocrine Treatment`, levels=c("First generation antiandrogens", "GNRH Agonists", "GNRH Agonists with 1st Generation ADT", 
                                                                      "GNRH / LHRH antagonists")))



# Change char to num
IRR_FOREST_ENDO_prostate_post = 
  IRR_FOREST_ENDO_prostate_post %>% mutate(
    estimate_num = as.numeric(estimate),
    lower_num = as.numeric(lower),
    upper_num = as.numeric(upper))

# Save this as a data object so that i can combine it into the forest plot of all lockdown periods
save(IRR_FOREST_ENDO_prostate_post, file=here(output.folder3, "IRR_FOREST_ENDO_prostate_post.RData"))

# Then combine with the data frame for all lockdown periods from other script.
load("~/GitHub/CancerCovidEndocrineTx/3_IRR/CPRDGold_202207_patched/EndoTxProstate/IRR_table_prostate_endo_post.Rdata")

IRR_FOREST_ENDO_PROSTATE_COMBINED_POST <- rbind(IRR_FOREST_ENDO_prostate, IRR_FOREST_ENDO_prostate_post)

IRR_FOREST_ENDO_PROSTATE_COMBINED_POST <- IRR_FOREST_ENDO_PROSTATE_COMBINED_POST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed", "Alllockdown"))) )



# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_ENDO_PROSTATE_PLOT_POST =
  ggplot(data=IRR_FOREST_ENDO_PROSTATE_COMBINED_POST, aes(x = `Lockdown Periods`,y = estimate_num, ymin = lower_num, ymax = upper_num ))+
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


IRR_FOREST_ENDO_PROSTATE_PLOT_POST

# Save

ggsave(here(output.folder4, "IRR_FOREST_ENDO_PROSTATE_PLOT_POST.tiff"), IRR_FOREST_ENDO_PROSTATE_PLOT_POST, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here(output.folder4, "IRR_FOREST_ENDO_PROSTATE_PLOT_POST.jpg"), IRR_FOREST_ENDO_PROSTATE_PLOT_POST, dpi=600, scale = 1.3,  width = 10, height = 8)



