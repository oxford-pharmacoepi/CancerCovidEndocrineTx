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


periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
#IRR <- list() # a list to store all the output later
rateratios <- vector("list",length(outcome)); names(rateratios) = outcome

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
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
rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists <- rateratios$`Aromatase Inhibitors with GnRH Agonists Or Antagonists`
rateratios_AromataseInhibitors <- rateratios$`Aromatase Inhibitors`
rateratios_TamoxifenGnRHAgonistsAntagonists <- rateratios$`Tamoxifen with GnRH Agonists Or Antagonists`
rateratios_Tamoxifen <- rateratios$Tamoxifen


################################################################################

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(periods, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(periods)
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

# it didn't complete this for tam with gnrh because there are only events in 5 of the lockdown periods and not 7. 
# need to extract this manually and create the IRR dataframe separately


IRR_Tamoxifen_with_GNRH_setup <- IR %>% filter(outcome=="Tamoxifen with GnRH Agonists Or Antagonists")

periods2 <- IRR_Tamoxifen_with_GNRH_setup%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome2 <-IRR_Tamoxifen_with_GNRH_setup%>% dplyr::select("outcome")%>% distinct()%>%pull()
rateratios2 <- vector("list",length(outcome2)); names(rateratios) = outcome2

for (y in 1:length(outcome2)){
  working.outcome <- outcome2[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods2)){ 
    working.period <- periods2[z]
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


rateratios_TamoxifenGnRHAgonistsAntagonists <- rateratios$`Tamoxifen with GnRH Agonists Or Antagonists`

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM THE LIST FOR TAMOXIFEN WITH GNRH WITH ONLY 5 PERIODS

get_IR_df_function2 <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(periods2, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(periods2)
  names(get_IR_df_t) <- get_IR_df_t[1,]
  get_IR_df_t <- get_IR_df_t[-1,]
  rownames(get_IR_df_t) <- paste(title)
  return(get_IR_df_t)
}

IRR_Tamoxifen_with_GNRH <- get_IR_df_function2(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
# ADD COLUMNS FOR MISSING PERIODS, WITH NAS
IRR_Tamoxifen_with_GNRH$`Second lockdown` <- c("NA")
IRR_Tamoxifen_with_GNRH$`Third lockdown` <- c("NA")
IRR_Tamoxifen_with_GNRH$`Post-lockdown1` <- c("NA")
IRR_Tamoxifen_with_GNRH$`Easing of restrictions` <- c("NA")
IRR_Tamoxifen_with_GNRH$`Legal restrictions removed` <- c("NA")
# JOIN THE TABLES
IRR_table_breast_endo <- rbind(IRR_AIs_with_GNRH, IRR_AIs, IRR_Tamoxifen_with_GNRH, IRR_Tamoxifen)
# REMOVE PRE-covid COLUMN
IRR_table_breast_endo <- IRR_table_breast_endo[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_breast_endo <- tibble::rownames_to_column(IRR_table_breast_endo, "Endocrine Treatment")
# RE-ORDER THE COLUMNS
IRR_table_breast_endo <- IRR_table_breast_endo[c(1,2,3,6,7,4,5)]

#### Save IRR
write.csv(IRR_table_breast_endo, file=here::here("3_IRR", "IRR_table_breast_endo.csv"))
save(IRR_table_breast_endo, file=here::here("3_IRR", "IRR_table_breast_endo.Rdata"))

#### Make pretty table
Pretty_IRR_table_breast_endo <- flextable(IRR_table_breast_endo) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of endocrine treatments in breast cancer patients over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_breast_endo' = Pretty_IRR_table_breast_endo, path=here("3_IRR", "Pretty_IRR_table_breast_endo.docx"))





# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON DAYS FROM ALL OF THE LISTS 

get_n_events_pd_function <- function(yourrateratiosname, title){
  
  neventspd <- as.data.frame(yourrateratiosname[[1]])
  neventspd <- neventspd %>%  mutate_if(is.numeric, round, digits=2)
  
  # remove last row of totals
  neventspd <- neventspd[-8,]
  # add a column to indicate the covid period
  neventspd <- cbind(periods, neventspd)
  
  # add column names
  names(neventspd)[1] <- "Periods"
  names(neventspd)[2] <- "N events"
  names(neventspd)[3] <- "Person Days"
  
  # combine person days with n events
  neventspd <- neventspd %>% mutate(`n events / person days` = paste0(paste(`N events`)," (", paste(`Person Days`), ")")) 
  
  # remove superfluous columns of events and person days
  neventspd <- neventspd[-c(2,3)]
  
  # transpose the table to have column headings as covid periods
  neventspd_t <- transpose(neventspd)
  #redefine row and column names
  colnames(neventspd_t) <- colnames(periods)
  names(neventspd_t) <- neventspd_t[1,]
  neventspd_t <- neventspd_t[-1,]
  rownames(neventspd_t) <- paste(title)
  return(neventspd_t)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
N_EVENTS_PD_AIs_with_GNRH <-  get_n_events_pd_function(rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists, "Aromatase Inhibitors with GnRH Agonists Or Antagonists")
N_EVENTS_PD_AIs <-  get_n_events_pd_function(rateratios_AromataseInhibitors, "Aromatase Inhibitors")
N_EVENTS_PD_Tamoxifen_with_GNRH <- get_n_events_pd_function(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
N_EVENTS_PD_Tamoxifen <-  get_n_events_pd_function(rateratios_Tamoxifen, "Tamoxifen")


# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM THE LIST FOR TAMOXIFEN WITH GNRH WITH ONLY 2 PERIODS

# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON DAYS FROM ALL OF THE LISTS 

get_n_events_pd_function2 <- function(yourrateratiosname, title){
  
  neventspd <- as.data.frame(yourrateratiosname[[1]])
  neventspd <- neventspd %>%  mutate_if(is.numeric, round, digits=2)
  
  # remove last row of totals
  neventspd <- neventspd[-8,]
  # add a column to indicate the covid period
  neventspd <- cbind(periods2, neventspd)
  
  # add column names
  names(neventspd)[1] <- "Periods"
  names(neventspd)[2] <- "N events"
  names(neventspd)[3] <- "Person Days"
  
  # combine person days with n events
  neventspd <- neventspd %>% mutate(`n events / person days` = paste0(paste(`N events`)," (", paste(`Person Days`), ")")) 
  
  # remove superfluous columns of events and person days
  neventspd <- neventspd[-c(2,3)]
  
  # transpose the table to have column headings as covid periods
  neventspd_t <- transpose(neventspd)
  #redefine row and column names
  colnames(neventspd_t) <- colnames(periods2)
  names(neventspd_t) <- neventspd_t[1,]
  neventspd_t <- neventspd_t[-1,]
  rownames(neventspd_t) <- paste(title)
  return(neventspd_t)
}

N_EVENTS_PD_Tamoxifen_with_GNRH <- get_IR_df_function2(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists")

# ADD COLUMNS FOR MISSING PERIODS, WITH NAS
N_EVENTS_PD_Tamoxifen_with_GNRH$`Second lockdown` <- c("NA")
N_EVENTS_PD_Tamoxifen_with_GNRH$`Third lockdown` <- c("NA")
N_EVENTS_PD_Tamoxifen_with_GNRH$`Post-lockdown1` <- c("NA")
N_EVENTS_PD_Tamoxifen_with_GNRH$`Easing of restrictions` <- c("NA")
N_EVENTS_PD_Tamoxifen_with_GNRH$`Legal restrictions removed` <- c("NA")


# JOIN THE TABLES
N_EVENTS_PD_table_endoTx_breast <- rbind(N_EVENTS_PD_AIs, N_EVENTS_PD_AIs_with_GNRH, N_EVENTS_PD_Tamoxifen, N_EVENTS_PD_Tamoxifen_with_GNRH)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PD_table_endoTx_breast <- tibble::rownames_to_column(N_EVENTS_PD_table_endoTx_breast, "Endocrine Treatment-Related Outcome")


#### Save n EVENTS AND PERSON DAYS
write.csv(N_EVENTS_PD_table_endoTx_breast, file=here::here("3_IRR", "N_EVENTS_PD_table_endoTx_breast.csv"))
save(N_EVENTS_PD_table_endoTx_breast, file=here::here("3_IRR", "N_EVENTS_PD_table_endoTx_breast.Rdata"))

#### Make pretty table
Pretty_N_EVENTS_PD_table_endoTx_breast <- flextable(N_EVENTS_PD_table_endoTx_breast) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of events and person days of endocrine treatments in breast cancer patients over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_N_EVENTS_PD_table_endoTx_breast' = Pretty_N_EVENTS_PD_table_endoTx_breast, path=here("3_IRR", "Pretty_N_EVENTS_PD_table_endoTx_breast.docx"))





# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function_CIs_Sep <- function(yourrateratiosname, title){
  
  `Endocrine Treatment` <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(`Endocrine Treatment`, periods, IR_CIS)
  
  return(IR_CIS)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
IRR_AIs_with_GNRH_Sep <-  get_IR_df_function_CIs_Sep(rateratios_AromataseInhibitorsGnRHAgonistsOrAntagonists, "Aromatase Inhibitors with GnRH Agonists Or Antagonists")
IRR_AIs_Sep <-  get_IR_df_function_CIs_Sep(rateratios_AromataseInhibitors, "Aromatase Inhibitors")
IRR_Tamoxifen_with_GNRH_Sep <- get_IR_df_function_CIs_Sep(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
IRR_Tamoxifen_Sep <-  get_IR_df_function_CIs_Sep(rateratios_Tamoxifen, "Tamoxifen")

# do this separately for tamoxifen with gnrh as only has 5 lockdown periods
get_IR_df_function_CIs_Sep2 <- function(yourrateratiosname, title){
  
  `Endocrine Treatment` <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(`Endocrine Treatment`, periods2, IR_CIS)
  
  return(IR_CIS)
}


IRR_Tamoxifen_with_GNRH_Sep <- get_IR_df_function_CIs_Sep2(rateratios_TamoxifenGnRHAgonistsAntagonists, "Tamoxifen with GnRH Agonists Or Antagonists") 
IRR_Tamoxifen_with_GNRH_Sep <- IRR_Tamoxifen_with_GNRH_Sep %>% rename("periods" = periods2)

# JOIN THE RATIO OUTPUTS   
IRR_FOREST_ENDO_BREAST <- rbind(IRR_AIs_with_GNRH_Sep, IRR_AIs_Sep, IRR_Tamoxifen_with_GNRH_Sep,  IRR_Tamoxifen_Sep)

# filter out pre-covid 
IRR_FOREST_ENDO_BREAST <- IRR_FOREST_ENDO_BREAST %>% filter(periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_ENDO_BREAST <- IRR_FOREST_ENDO_BREAST %>% rename("Lockdown Periods" = periods) 


IRR_FOREST_ENDO_BREAST <- IRR_FOREST_ENDO_BREAST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-lockdown1", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_ENDO_BREAST_PLOT =
  ggplot(data=IRR_FOREST_ENDO_BREAST, aes(x = `Lockdown Periods`,y = estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Endocrine Treatment')+ ylab("Incidence Rate Ratio (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
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


IRR_FOREST_ENDO_BREAST_PLOT

# Save

ggsave(here("3_IRR", "IRR_FOREST_ENDO_BREAST_PLOT.tiff"), IRR_FOREST_ENDO_BREAST_PLOT, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here("3_IRR", "IRR_FOREST_ENDO_BREAST_PLOT.jpg"), IRR_FOREST_ENDO_BREAST_PLOT, dpi=600, scale = 1.3,  width = 10, height = 8)





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


write.csv(ir_ci, file=here("3_IRR", "IR_table_breast_endo.csv"))
save(ir_ci, file=here("3_IRR", "IR_table_breast_endo.RData"))



# add combined periods post-lockdown - this gives you all the IR calculated anytime after lockdown.These are not averaged but caluclated
overall.post <-IR.overall%>% 
  filter(months.since.start >=43)%>%
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


write.csv(ir.post_ci, file=here("3_IRR", "IR_table_breast_endo_with_post_lockdown.csv"))
save(ir.post_ci, file=here("3_IRR", "IR_table_breast_endo_with_post_lockdown.RData"))


# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci, ir.post_ci)

write.csv(ir_ci_pre_post, file=here("3_IRR", "IR_table_breast_endo_with_pre_post_lockdown.csv"))
save(ir_ci_pre_post, file=here("3_IRR", "IR_table_breast_endo_with_pre_post_lockdown.RData"))

# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 

ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(1, 6,4,5,7,8,2,3,9)]
#ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(2,4,8,10,12,13,3,6,14,5,7,9,1,11), c(1, 2, 5, 9, 6, 7, 8, 3,4)]
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename("Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (July 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-first lockdown 1 (July 2020-Oct 2020)" = "Post-lockdown1",
                                                        "Second lockdown (Nov 2020-Dec 2020)" = "Second lockdown", 
                                                        "Third lockdown (Jan 2021-Feb 2021)" = "Third lockdown",
                                                        "Easing of restrictions (March 2021-June 2021" = "Easing of restrictions", 
                                                        "Legal restrictions removed (July 2021-Dec 2021)"= "Legal restrictions removed")


Pretty_observed_IR_results_breast_endo <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of endocrine treatments in breast cancer patients in each of the time periods") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here("3_IRR", "IR_table_breast_endo_with_pre_post_lockdown_pivot.RData"))
write.csv(ir_ci_pre_post_pivot, file=here("3_IRR", "IR_table_breast_endo_with_pre_post_lockdown_pivot.csv"))

save_as_docx('Pretty_observed_IR_results_breast_endo' = Pretty_observed_IR_results_breast_endo, path=here("3_IRR", "Pretty_obs_IR_results_breast_endo.docx"))




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


periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
#IRR <- list() # a list to store all the output later
rateratios <- vector("list",length(outcome)); names(rateratios) = outcome

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
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
rateratios_First_gen_antiandrogens <- rateratios$`First generation antiandrogens`
rateratios_GNRH_Agonists_with_1st_Gen_ADT <- rateratios$`GNRH Agonists with 1st Generation ADT`
rateratios_GNRH_Agonists <- rateratios$`GNRH Agonists`
rateratios_GNRH_LHRHantagonists <- rateratios$`GNRH / LHRH antagonists`


################################################################################

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(periods, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(periods)
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
IRR_table_prostate_endo <- rbind(IRR_First_gen_antiandrogens, IRR_GNRH_Agonists, IRR_GNRH_Agonists_with_1st_Gen_ADT, IRR_GNRH_LHRHantagonists)
# REMOVE PRE-covid COLUMN
IRR_table_prostate_endo <- IRR_table_prostate_endo[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_prostate_endo <- tibble::rownames_to_column(IRR_table_prostate_endo, "Endocrine Treatment")

#### Save IRR
write.csv(IRR_table_prostate_endo, file=here::here("3_IRR", "IRR_table_prostate_endo.csv"))
save(IRR_table_prostate_endo, file=here::here("3_IRR", "IRR_table_prostate_endo.Rdata"))

#### Make pretty table
Pretty_IRR_table_prostate_endo <- flextable(IRR_table_prostate_endo) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of endocrine treatments in prostate cancer patients over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_prostate_endo' = Pretty_IRR_table_prostate_endo, path=here("3_IRR", "Pretty_IRR_table_prostate_endo.docx"))





# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON DAYS FROM ALL OF THE LISTS 

get_n_events_pd_function <- function(yourrateratiosname, title){
  
  neventspd <- as.data.frame(yourrateratiosname[[1]])
  neventspd <- neventspd %>%  mutate_if(is.numeric, round, digits=2)
  
  # remove last row of totals
  neventspd <- neventspd[-8,]
  # add a column to indicate the covid period
  neventspd <- cbind(periods, neventspd)
  
  # add column names
  names(neventspd)[1] <- "Periods"
  names(neventspd)[2] <- "N events"
  names(neventspd)[3] <- "Person Days"
  
  # combine person days with n events
  neventspd <- neventspd %>% mutate(`n events / person days` = paste0(paste(`N events`)," (", paste(`Person Days`), ")")) 
  
  # remove superfluous columns of events and person days
  neventspd <- neventspd[-c(2,3)]
  
  # transpose the table to have column headings as covid periods
  neventspd_t <- transpose(neventspd)
  #redefine row and column names
  colnames(neventspd_t) <- colnames(periods)
  names(neventspd_t) <- neventspd_t[1,]
  neventspd_t <- neventspd_t[-1,]
  rownames(neventspd_t) <- paste(title)
  return(neventspd_t)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS

N_EVENTS_PD_First_gen_antiandrogens <-  get_n_events_pd_function(rateratios_First_gen_antiandrogens, "First generation antiandrogens")
N_EVENTS_PD_GNRH_Agonists_with_1st_Gen_ADT <-  get_n_events_pd_function(rateratios_GNRH_Agonists_with_1st_Gen_ADT, "GNRH Agonists with 1st Generation ADT")
N_EVENTS_PD_GNRH_Agonists <- get_n_events_pd_function(rateratios_GNRH_Agonists, "GNRH Agonists") 
N_EVENTS_PD_GNRH_LHRHantagonists <-  get_n_events_pd_function(rateratios_GNRH_LHRHantagonists, "GNRH / LHRH antagonists")


# JOIN THE TABLES
N_EVENTS_PD_table_endo_prostate <- rbind(N_EVENTS_PD_First_gen_antiandrogens, N_EVENTS_PD_GNRH_Agonists, N_EVENTS_PD_GNRH_Agonists_with_1st_Gen_ADT, N_EVENTS_PD_GNRH_LHRHantagonists)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PD_table_endo_prostate <- tibble::rownames_to_column(N_EVENTS_PD_table_endo_prostate, "Endocrine Treatment")


#### Save n EVENTS AND PERSON DAYS
write.csv(N_EVENTS_PD_table_endo_prostate, file=here::here("3_IRR", "N_EVENTS_PD_table_endo_prostate.csv"))
save(N_EVENTS_PD_table_endo_prostate, file=here::here("3_IRR", "N_EVENTS_PD_table_endo_prostate.Rdata"))

#### Make pretty table
Pretty_N_EVENTS_PD_table_endo_prostate <- flextable(N_EVENTS_PD_table_endo_prostate) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of events and person days of endocrine treatments in prostate cancer patients over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_N_EVENTS_PD_table_endo_prostate' = Pretty_N_EVENTS_PD_table_endo_prostate, path=here("3_IRR", "Pretty_N_EVENTS_PD_table_endo_prostate.docx"))



# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function_CIs_Sep <- function(yourrateratiosname, title){
  
  `Endocrine Treatment` <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(`Endocrine Treatment`, periods, IR_CIS)
  
  return(IR_CIS)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS
IRR_First_gen_antiandrogens_Sep <-  get_IR_df_function_CIs_Sep(rateratios_First_gen_antiandrogens, "First generation antiandrogens")
IRR_GNRH_Agonists_with_1st_Gen_ADT_Sep <-  get_IR_df_function_CIs_Sep(rateratios_GNRH_Agonists_with_1st_Gen_ADT, "GNRH Agonists with 1st Generation ADT")
IRR_GNRH_Agonists_Sep <- get_IR_df_function_CIs_Sep(rateratios_GNRH_Agonists, "GNRH Agonists") 
IRR_GNRH_LHRHantagonists_Sep <-  get_IR_df_function_CIs_Sep(rateratios_GNRH_LHRHantagonists, "GNRH / LHRH antagonists")



# JOIN THE RATIO OUTPUTS   
IRR_FOREST_ENDO_prostate <- rbind(IRR_First_gen_antiandrogens_Sep, IRR_GNRH_Agonists_Sep, IRR_GNRH_Agonists_with_1st_Gen_ADT_Sep, IRR_GNRH_LHRHantagonists_Sep)

# filter out pre-covid 
IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate %>% filter(periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate %>% rename("Lockdown Periods" = periods) 


IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-lockdown1", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate  %>%
  mutate(`Endocrine Treatment` = factor(`Endocrine Treatment`, levels=c("First generation antiandrogens", "GNRH Agonists", "GNRH Agonists with 1st Generation ADT", 
                                                                      "GNRH / LHRH antagonists")))


# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_ENDO_prostate_PLOT =
  ggplot(data=IRR_FOREST_ENDO_prostate, aes(x = `Lockdown Periods`,y = estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Endocrine Treatment')+ ylab("Incidence Rate Ratio (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
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


IRR_FOREST_ENDO_prostate_PLOT

# Save

ggsave(here("3_IRR", "IRR_FOREST_ENDO_prostate_PLOT.tiff"), IRR_FOREST_ENDO_prostate_PLOT, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here("3_IRR", "IRR_FOREST_ENDO_prostate_PLOT.jpg"), IRR_FOREST_ENDO_prostate_PLOT, dpi=600, scale = 1.3,  width = 10, height = 8)




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


write.csv(ir_ci, file=here("3_IRR", "IR_table_prostate_endo.csv"))
save(ir_ci, file=here("3_IRR", "IR_table_prostate_endo.RData"))



# add combined periods post-lockdown - this gives you all the IR calculated anytime after lockdown.These are not averaged but caluclated
overall.post <-IR.overall%>% 
  filter(months.since.start >=43)%>%
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


write.csv(ir.post_ci, file=here("3_IRR", "IR_table_prostate_endo_with_post_lockdown.csv"))
save(ir.post_ci, file=here("3_IRR", "IR_table_prostate_endo_with_post_lockdown.RData"))


# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci, ir.post_ci)

write.csv(ir_ci_pre_post, file=here("3_IRR", "IR_table_prostate_endo_with_pre_post_lockdown.csv"))
save(ir_ci_pre_post, file=here("3_IRR", "IR_table_prostate_endo_with_pre_post_lockdown.RData"))

# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 
# re-order columns
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(1, 6,4,5,7,8,2,3,9)]
# re-order rows
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(1,3,4,2),]
#ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(2,4,8,10,12,13,3,6,14,5,7,9,1,11), c(1, 2, 5, 9, 6, 7, 8, 3,4)]
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename("Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (July 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-first lockdown 1 (July 2020-Oct 2020)" = "Post-lockdown1",
                                                        "Second lockdown (Nov 2020-Dec 2020)" = "Second lockdown", 
                                                        "Third lockdown (Jan 2021-Feb 2021)" = "Third lockdown",
                                                        "Easing of restrictions (March 2021-June 2021" = "Easing of restrictions", 
                                                        "Legal restrictions removed (July 2021-Dec 2021)"= "Legal restrictions removed")


Pretty_observed_IR_results_prostate_endo <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of endocrine treatments in prostate cancer patients in each of the time periods") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here("3_IRR", "IR_table_prostate_endo_with_pre_post_lockdown_pivot.RData"))
write.csv(ir_ci_pre_post_pivot, file=here("3_IRR", "IR_table_prostate_endo_with_pre_post_lockdown_pivot.csv"))

save_as_docx('Pretty_observed_IR_results_prostate_endo' = Pretty_observed_IR_results_prostate_endo, path=here("3_IRR", "Pretty_obs_IR_results_prostate_endo.docx"))




