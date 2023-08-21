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

# ===================== BREAST CANCER ======================================== #

# Load the cleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

inc_data <- read_csv("0_DataPrep/inc_data_endo_dx_outcomes_in_prostate.csv")


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
rateratios_Osteopenia <- rateratios$`Osteopenia`
rateratios_Osteoporosis <- rateratios$`Osteoporosis`
rateratios_Bisphosphonates <- rateratios$`Bisphosphonates`
rateratios_BoneFracture <- rateratios$`Bone Fracture`


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

IRR_Osteopenia  <-  get_IR_df_function(rateratios_Osteopenia, "Osteopenia")
IRR_Osteoporosis <-  get_IR_df_function(rateratios_Osteoporosis, "Osteoporosis")
IRR_Bisphosphonates <- get_IR_df_function(rateratios_Bisphosphonates, "Bisphosphonates") 
IRR_BoneFracture <-  get_IR_df_function(rateratios_BoneFracture, "Bone Fracture")

# JOIN THE TABLES
IRR_table_endodx_prostate <- rbind(IRR_Bisphosphonates, IRR_BoneFracture)
# REMOVE PRE-covid COLUMN
IRR_table_endodx_prostate <- IRR_table_endodx_prostate[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_endodx_prostate <- tibble::rownames_to_column(IRR_table_endodx_prostate, "Endocrine Treatment")


#### Save IRR
write.csv(IRR_table_endodx_prostate, file=here::here("3_IRR", "IRR_table_endodx_prostate.csv"))
save(IRR_table_endodx_prostate, file=here::here("3_IRR", "IRR_table_endodx_prostate.Rdata"))

#### Make pretty table
Pretty_IRR_table_endodx_prostate <- flextable(IRR_table_endodx_prostate) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of treatment-related outcomes in prostate cancer patients on endocrine treatments over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_endodx_prostate' = Pretty_IRR_table_endodx_prostate, path=here("3_IRR", "Pretty_IRR_table_endodx_prostate.docx"))



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

N_EVENTS_PD_Osteopenia  <-  get_n_events_pd_function(rateratios_Osteopenia, "Osteopenia")
N_EVENTS_PD_Osteoporosis <-  get_n_events_pd_function(rateratios_Osteoporosis, "Osteoporosis")
N_EVENTS_PD_Bisphosphonates <- get_n_events_pd_function(rateratios_Bisphosphonates, "Bisphosphonates") 
N_EVENTS_PD_BoneFracture <-  get_n_events_pd_function(rateratios_BoneFracture, "Bone Fracture")


# JOIN THE TABLES
N_EVENTS_PD_table_endodx_prostate <- rbind(N_EVENTS_PD_Bisphosphonates, N_EVENTS_PD_BoneFracture)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PD_table_endodx_prostate <- tibble::rownames_to_column(N_EVENTS_PD_table_endodx_prostate, "Endocrine Treatment-Related Outcome")


#### Save n EVENTS AND PERSON DAYS
write.csv(N_EVENTS_PD_table_endodx_prostate, file=here::here("3_IRR", "N_EVENTS_PD_table_endodx_prostate.csv"))
save(N_EVENTS_PD_table_endodx_prostate, file=here::here("3_IRR", "N_EVENTS_PD_table_endodx_prostate.Rdata"))

#### Make pretty table
Pretty_N_EVENTS_PD_table_endodx_prostate <- flextable(N_EVENTS_PD_table_endodx_prostate) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of events and person days of treatment-related outcomes in prostate cancer patients on endocrine treatments over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_N_EVENTS_PD_table_endodx_prostate' = Pretty_N_EVENTS_PD_table_endodx_prostate, path=here("3_IRR", "Pretty_N_EVENTS_PD_table_endodx_prostate.docx"))


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
IRR_Bisphosphonates_Sep <- get_IR_df_function_CIs_Sep(rateratios_Bisphosphonates, "Bisphosphonates") 
IRR_BoneFracture_Sep <-  get_IR_df_function_CIs_Sep(rateratios_BoneFracture, "Bone Fracture")


# JOIN THE RATIO OUTPUTS   
IRR_FOREST_endodx_prostate <- rbind(IRR_Bisphosphonates_Sep,  IRR_BoneFracture_Sep)

# filter out pre-covid 
IRR_FOREST_endodx_prostate <- IRR_FOREST_endodx_prostate %>% filter(periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_endodx_prostate <- IRR_FOREST_endodx_prostate %>% rename("Lockdown Periods" = periods) 


IRR_FOREST_endodx_prostate <- IRR_FOREST_endodx_prostate  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-lockdown1", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_endodx_prostate_plot =
  ggplot(data=IRR_FOREST_endodx_prostate, aes(x = `Lockdown Periods`,y = estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Endocrine Treatment Outcomes in Prostate Cancer Patients on Endocrine Treatments')+ ylab("Incidence Rate Ratio (95% Confidence Interval - Pre-Pandemic as reference)")+
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


IRR_FOREST_endodx_prostate_plot

# Save

ggsave(here("3_IRR", "IRR_FOREST_endodx_prostate_plot.tiff"), IRR_FOREST_endodx_prostate_plot, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here("3_IRR", "IRR_FOREST_endodx_prostate_plot.jpg"), IRR_FOREST_endodx_prostate_plot, dpi=600, scale = 1.3,  width = 10, height = 8)

