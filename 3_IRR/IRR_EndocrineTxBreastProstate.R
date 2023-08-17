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

# JOIN THE TABLES
IRR_table_breast_endo <- rbind(IRR_AIs_with_GNRH, IRR_AIs, IRR_Tamoxifen_with_GNRH, IRR_Tamoxifen)
# REMOVE PRE-covid COLUMN
IRR_table_breast_endo <- IRR_table_breast_endo[-2]
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
IRR_table_prostate_endo <- rbind(IRR_First_gen_antiandrogens, IRR_GNRH_Agonists_with_1st_Gen_ADT, IRR_GNRH_Agonists, IRR_GNRH_LHRHantagonists)
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
IRR_FOREST_ENDO_prostate <- rbind(IRR_First_gen_antiandrogens_Sep, IRR_GNRH_Agonists_with_1st_Gen_ADT_Sep, IRR_GNRH_Agonists_Sep,  IRR_GNRH_LHRHantagonists_Sep)

# filter out pre-covid 
IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate %>% filter(periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate %>% rename("Lockdown Periods" = periods) 


IRR_FOREST_ENDO_prostate <- IRR_FOREST_ENDO_prostate  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-lockdown1", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

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



