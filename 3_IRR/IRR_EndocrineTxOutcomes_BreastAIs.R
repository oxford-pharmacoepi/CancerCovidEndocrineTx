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
IRR_table_endodx_breastAI <- rbind(IRR_Osteopenia, IRR_Osteoporosis, IRR_Bisphosphonates, IRR_BoneFracture)
# REMOVE PRE-covid COLUMN
IRR_table_endodx_breastAI <- IRR_table_endodx_breastAI[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_endodx_breastAI <- tibble::rownames_to_column(IRR_table_endodx_breastAI, "Endocrine Treatment")
IRR_table_endodx_breastAI <- IRR_table_endodx_breastAI[c(3,4,1,2),]

#### Save IRR
write.csv(IRR_table_endodx_breastAI, file=here::here(output.folder5, "IRR_table_endodx_breastAI.csv"))
save(IRR_table_endodx_breastAI, file=here::here(output.folder5, "IRR_table_endodx_breastAI.Rdata"))

#### Make pretty table
Pretty_IRR_table_endodx_breastAI <- flextable(IRR_table_endodx_breastAI) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of treatment-related outcomes in breast cancer patients on aromatase inhibitors over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_endodx_breastAI' = Pretty_IRR_table_endodx_breastAI, path=here(output.folder5, "Pretty_IRR_table_endodx_breastAI.docx"))



# FUNCTION TO EXTRACT ALL THE N EVENTS AND PERSON MONTHS FROM ALL OF THE LISTS 

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
  names(neventspd)[3] <- "Person MONTHS"
  
  # combine person MONTHS with n events
  neventspd <- neventspd %>% mutate(`n events / person MONTHS` = paste0(paste(`N events`)," (", paste(`Person MONTHS`), ")")) 
  
  # remove superfluous columns of events and person MONTHS
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
N_EVENTS_PD_table_endodx_breastAI <- rbind(N_EVENTS_PD_Osteopenia, N_EVENTS_PD_Osteoporosis, N_EVENTS_PD_Bisphosphonates, N_EVENTS_PD_BoneFracture)
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
N_EVENTS_PD_table_endodx_breastAI <- tibble::rownames_to_column(N_EVENTS_PD_table_endodx_breastAI, "Endocrine Treatment-Related Outcome")
# re-order the rows
N_EVENTS_PD_table_endodx_breastAI <- N_EVENTS_PD_table_endodx_breastAI[c(3,4,1,2),]

#### Save n EVENTS AND PERSON MONTHS
write.csv(N_EVENTS_PD_table_endodx_breastAI, file=here::here(output.folder5, "N_EVENTS_PD_table_endodx_breastAI.csv"))
save(N_EVENTS_PD_table_endodx_breastAI, file=here::here(output.folder5, "N_EVENTS_PD_table_endodx_breastAI.Rdata"))

#### Make pretty table
Pretty_N_EVENTS_PD_table_endodx_breastAI <- flextable(N_EVENTS_PD_table_endodx_breastAI) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of events and person MONTHS of treatment-related outcomes in breast cancer patients on aromatase inhibitors over the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_N_EVENTS_PD_table_endodx_breastAI' = Pretty_N_EVENTS_PD_table_endodx_breastAI, path=here(output.folder5, "Pretty_N_EVENTS_PD_table_endodx_breastAI.docx"))



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
IRR_Osteopenia_Sep  <-  get_IR_df_function_CIs_Sep(rateratios_Osteopenia, "Osteopenia")
IRR_Osteoporosis_Sep <-  get_IR_df_function_CIs_Sep(rateratios_Osteoporosis, "Osteoporosis")
IRR_Bisphosphonates_Sep <- get_IR_df_function_CIs_Sep(rateratios_Bisphosphonates, "Bisphosphonates") 
IRR_BoneFracture_Sep <-  get_IR_df_function_CIs_Sep(rateratios_BoneFracture, "Bone Fracture")


# JOIN THE RATIO OUTPUTS   
IRR_FOREST_endodx_breastAI <- rbind(IRR_Osteopenia_Sep, IRR_Osteoporosis_Sep, IRR_Bisphosphonates_Sep,  IRR_BoneFracture_Sep)

# filter out pre-covid 
IRR_FOREST_endodx_breastAI <- IRR_FOREST_endodx_breastAI %>% filter(periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST_endodx_breastAI <- IRR_FOREST_endodx_breastAI %>% rename("Lockdown Periods" = periods) 

# RENAME POST-LOCKDOWN 1
IRR_FOREST_endodx_breastAI$`Lockdown Periods`[IRR_FOREST_endodx_breastAI$`Lockdown Periods` == "Post-lockdown1"] <- "Post-first lockdown"

IRR_FOREST_endodx_breastAI <- IRR_FOREST_endodx_breastAI  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown", "Second lockdown", 
                                                                      "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )


# FILTER OUT BONE FRACTURE IF NOT INCLUDING
IRR_FOREST_endodx_breastAI <- IRR_FOREST_endodx_breastAI %>% filter(`Endocrine Treatment` !="Bone Fracture")


# Change char to num
IRR_FOREST_endodx_breastAI = 
  IRR_FOREST_endodx_breastAI %>% mutate(
    estimate_num = as.numeric(estimate),
    lower_num = as.numeric(lower),
    upper_num = as.numeric(upper))

save(IRR_FOREST_endodx_breastAI, file=here(output.folder5, "IRR_FOREST_endodx_breastAI.RData"))


# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_FOREST_endodx_breastAI_plot_ex_bf =
  ggplot(data=IRR_FOREST_endodx_breastAI, aes(x = `Lockdown Periods`,y = estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Endocrine Treatment Outcome in Breast Cancer Patients on Aromatase Inhibitors')+ ylab("Incidence Rate Ratio on Logarithmic Scale (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
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


IRR_FOREST_endodx_breastAI_plot_ex_bf

# Save

ggsave(here(output.folder5, "IRR_FOREST_endodx_breastAI_plot_ex_bf.tiff"), IRR_FOREST_endodx_breastAI_plot_ex_bf, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here(output.folder5, "IRR_FOREST_endodx_breastAI_plot_ex_bf.jpg"), IRR_FOREST_endodx_breastAI_plot_ex_bf, dpi=600, scale = 1.3,  width = 10, height = 8)



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


write.csv(ir_ci, file=here(output.folder5, "IR_table_endodx_breastAI.csv"))
save(ir_ci, file=here(output.folder5, "IR_table_endodx_breastAI.RData"))



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


Pretty_observed_IR_results_endodx_breastAI <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of endocrine-treatment related outcomes in breast cancer patients on aromatase inhibitors in each of the time periods") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here(output.folder5, "IR_table_endodx_breastAI_with_pre_post_lockdown_pivot.RData"))
write.csv(ir_ci_pre_post_pivot, file=here(output.folder5, "IR_table_endodx_breastAI_with_pre_post_lockdown_pivot.csv"))

save_as_docx('Pretty_observed_IR_results_endodx_breastAI' = Pretty_observed_IR_results_endodx_breastAI, path=here(output.folder5, "Pretty_obs_IR_results_endodx_breastAI.docx"))




