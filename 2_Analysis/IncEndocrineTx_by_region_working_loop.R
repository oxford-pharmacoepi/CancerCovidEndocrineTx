# ============================================================================ #
#                       Incidence of Endocrine Treatments                      #
#                            Stratified by Region                              #
#                       LOOPS OVER REGION TO DERIVE INCIDENCE                  #
#                              Nicola Barclay                                  #
#                                22-11-2023                                    #
# ============================================================================ #

# compute the denominator first

cdm <-generateDenominatorCohortSet(
  cdm = cdm,
  #name = denominator_all,
  #strataTable = strata_table_name_1,
  #strataCohortId = 1,
  cohortDateRange = as.Date(c("2017-01-01","2020-03-22")),
  ageGroup = list(c(0,150)),
  sex = c("Both"),
  daysPriorHistory = 365,
  temporary = FALSE
)

## ======= Join the CDM denominator with location information ======== ##

allRegions <- cdm$location %>% pull(location_source_value)
cdm$denominator_original <- cdm$denominator
cdm$denominator <- cdm$denominator_original %>% rename("person_id" = "subject_id") %>%
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  mutate(region_collapsed = case_when(region %in% c("Yorkshire  & The Humber", "East Midlands", 
                                                    "West Midlands", "North East", "North West", "East of England", "London", 
                                                    "South East", "South West") ~ "England",
                                      region == "Northern Ireland" ~ "Northern Ireland",
                                      region == "Scotland" ~ "Scotland",
                                      region == "Wales" ~ "Wales"))

cdm$denominator <- cdm$denominator %>% rename("subject_id" = "person_id")

# filter the cdm denominator by region
cdm$denominator_eng <- cdm$denominator %>% filter(region_collapsed %in% "England")
cdm$denominator_NI <- cdm$denominator %>% filter(region_collapsed %in% "Northern Ireland")
cdm$denominator_scot <- cdm$denominator %>% filter(region_collapsed %in% "Scotland")
cdm$denominator_wales <- cdm$denominator %>% filter(region_collapsed %in% "Wales")

# check numbers in the denominators
cdm$denominator_eng %>% tally()  # to check numbers in denominator population
cdm$denominator_NI %>% tally()  # to check numbers in denominator population
cdm$denominator_scot %>% tally()  # to check numbers in denominator population
cdm$denominator_wales %>% tally()  # to check numbers in denominator population

print(paste0("- Got denominators"))
info(logger, "- Got denominators")



## ================= CALCULATE INCIDENCE - LOOP OVER ALL REGIONS ======================== ##

print(paste0("- Getting incidence: Endocrine Tx looped across all regions"))
info(logger, "- Getting incidence: Endocrine Tx looped across all regions")

# pull the denominator names into a dataframe - this is what you will loop over
denom_regions <- c("denominator_eng", "denominator_NI", "denominator_scot", "denominator_wales")

# create empty list to put all the results in and give it the names of the variable you wish to loop over
listofincs <- vector("list",length(denom_regions)); names(listofincs) = denom_regions


for (y in 1:length(denom_regions)){
  working.denom <- denom_regions[y]
  
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = working.denom,
  outcomeTable = outcome_table_name_1, 
  interval = "overall",
  completeDatabaseIntervals = FALSE,
  outcomeWashout = Inf,
  repeatedEvents = FALSE,
  minCellCount = 5,
  temporary = TRUE)

listofincs[[y]] <- inc
}

save(listofincs, file = here("Results", db.name, "Regions", "listofincs_by_region.RData"))

# EXTRACT THE INCIDENCE RATES PER REGION FROM THE LIST AND SAVE AS RDATA OBJECTS
inc_england <- listofincs$denominator_eng
inc_NI <- listofincs$denominator_NI
inc_scotland <- listofincs$denominator_scot
inc_wales <- listofincs$denominator_wales

print(paste0("- Got incidence: Endocrine Tx looped over regions"))
info(logger, "- Got incidence: Endocrine Tx looped over regions")


## ======== EXPORT ALL INCIDENCE  RESULTS ===================== ##

print(paste0("- Exporting incidence and prevalence results: england cancer populations"))
info(logger, "- Exporting incidence and Prevalence results: england cancer populations")


exportIncidencePrevalenceResults(resultList=list("inc_england" = inc_england, "inc_NI" = inc_NI, 
                                                 "inc_scotland" = inc_scotland, "inc_wales" = inc_wales), 
                                 zipName=paste0(db.name, "IncEndocrineTxResults_by_region"),
                                 outputFolder=here("Results", db.name, "Regions")) 

print(paste0("- Exported incidence results by region"))
info(logger, "- Exported incidence  results by region")


## ==================== PLOTS =============================================== ##


library(ggpubr)

#ADD COLUMN OF REGION TO THE INCIDENCE OBJECTS
inc_england['Region']='England'
inc_NI['Region']='Northern Ireland'
inc_scotland['Region']='Scotland'
inc_wales['Region']='Wales'


# BIND ALL THE DATAFRAMES TOGETHER

IR_outcomes_by_region <- rbind(inc_england, inc_NI, inc_scotland, inc_wales)

# PLOT THE OVERALL INCIDENCE RATES FOR EACH CANCER SEPARATELY (so that can filter out females for prostate and males for breast) 
# WITH REGIONS ON DIFFERENT LINES  

IR_outcomes_by_region_plot <- IR_outcomes_by_region %>%
  ggplot(aes(x = Region, y=incidence_100000_pys,
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper, color=outcome_cohort_name, group=outcome_cohort_name)) +
  geom_point() + geom_line() +
  geom_errorbar(width=0) +
  scale_y_continuous(limits = c(0, NA)) +
  #ggtitle("") +
  labs(colour = "Outcomes", x=" " , y="Incidence per 100,000 person-years") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.margin=grid::unit(c(1,1,0,1), "cm"))

IR_outcomes_by_region_plot


# Save the plot as jpg
ggsave(here("Results", db.name , "3_Regions", "inc_regions.jpg"), IR_outcomes_by_region_plot, dpi=900, scale = 1, width = 12, height = 9)


