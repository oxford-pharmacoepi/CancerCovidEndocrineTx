# ============================================================================ #
#                   Code to examine region distribution of                     #
#                   breast and prostate cancer strata cohorts                  #
#                         for endcrine treatment paper                         #
#                              Nicola Barclay                                  #
#                                17-10-2023                                    #
# ============================================================================ #

# You will need the breast and prostate cancer strata populations instantiated

# count n in each cohort definition
cdm[[strata_table_name_1]] %>% group_by(cohort_definition_id) %>% tally() 



## ======= Join the CDM breast and prostate strata table with location information ======== ##


breast_prostate_regions <- cdm$cancercovid_endotx_breast_prostate_strata %>% 
  rename("person_id" = "subject_id") %>%
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  #filter(region == !!allRegions[1]) %>%
  mutate(region_collapsed = case_when(region %in% c("Yorkshire  & The Humber", "East Midlands", 
                                                    "West Midlands", "North East", "North West", "East of England", "London", 
                                                    "South East", "South West") ~ "England",
                                      region == "Northern Ireland" ~ "Northern Ireland",
                                      region == "Scotland" ~ "Scotland",
                                      region == "Wales" ~ "Wales")) %>%
  group_by(cohort_definition_id, region_collapsed) %>% 
  tally() %>% 
  arrange(cohort_definition_id) %>%
  collect() 


## ======= Examine the proportions of the breast and prostate cohorts coming from each region ====== ##



write.csv(region_distribution_cancer_cohorts, file=here::here("Results", db.name, "Regions", "region_distribution_cancer_cohorts.csv"))
save(region_distribution_cancer_cohorts, file=here::here("Results", db.name, "Regions", "region_distribution_cancer_cohorts.RData"))

# CREATE PROPORTIONS COLUMN IN THE TABLE SPLIT BY COHORT AND REGION
# 
# C1 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==1)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C2 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==2)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C3 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==3)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C4 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==4)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C5 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==5)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C6 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==6)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C7 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==7)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C8 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==8)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C9 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==9)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C10 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==10)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C11 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==11)%>% mutate(freq = scales::label_percent()(n / sum(n)))
# C12 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==12)%>% mutate(freq = scales::label_percent()(n / sum(n)))


# create a tibble of all 4 regions to later join with all regions so that you can populate cells with NA when missing rather than omitting the row
region_names <- tibble::as_tibble_col(c("England", "Wales", "Scotland", "Northern Ireland"), column_name = "region")

C1 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==1)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 1)
C2 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==2)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 2)
C3 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==3)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 3)
C4 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==4)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 4)
C5 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==5)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 5)
C6 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==6)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 6)
C7 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==7)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 7)
C8 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==8)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 8)
C9 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==9)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 9)
C10 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==10)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 10)
C11 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==11)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 11)
C12 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==12)%>% mutate(freq = (n/sum(n))*100) #%>% right_join(region_names) %>% mutate(cohort_definition_id = 12)


Proportions_regions_cancer_cohorts <- bind_rows(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12)

# LABEL THE COHORTS FOR VISUALISATION
Proportions_regions_cancer_cohorts <-  Proportions_regions_cancer_cohorts %>% mutate(Cancer = case_when(cohort_definition_id <=3 ~ "Breast",
                                                                                                        (cohort_definition_id >=4)&(cohort_definition_id <=6)~"Colorectal",
                                                                                                        (cohort_definition_id >=7)&(cohort_definition_id <=9)~"Lung",
                                                                                                        (cohort_definition_id >=10)&(cohort_definition_id <=12)~"Prostate"))
Proportions_regions_cancer_cohorts <-  Proportions_regions_cancer_cohorts %>% mutate(Time = case_when(cohort_definition_id == 1 ~ "Before lockdown",
                                                                                                      cohort_definition_id == 4 ~ "Before lockdown",
                                                                                                      cohort_definition_id == 7 ~ "Before lockdown",
                                                                                                      cohort_definition_id == 10 ~ "Before lockdown",
                                                                                                      cohort_definition_id == 2 ~ "During lockdown",
                                                                                                      cohort_definition_id == 5 ~ "During lockdown",
                                                                                                      cohort_definition_id == 8 ~ "During lockdown",
                                                                                                      cohort_definition_id == 11 ~ "During lockdown",
                                                                                                      cohort_definition_id == 3 ~ "After lockdown",
                                                                                                      cohort_definition_id == 6 ~ "After lockdown",
                                                                                                      cohort_definition_id == 9 ~ "After lockdown",
                                                                                                      cohort_definition_id == 12 ~ "After lockdown"))


Proportions_regions_cancer_cohorts$cohort_name <- paste(Proportions_regions_cancer_cohorts$Cancer, Proportions_regions_cancer_cohorts$Time, sep="_")

# RENAME REGIONS_COLLAPSED
Proportions_regions_cancer_cohorts <- Proportions_regions_cancer_cohorts %>% rename(Region = "region_collapsed") 

write.csv(Proportions_regions_cancer_cohorts, file=here::here("Results", db.name, "Regions", "Proportions_regions_cancer_cohorts.csv"))
save(Proportions_regions_cancer_cohorts, file=here::here("Results", db.name, "Regions", "Proportions_regions_cancer_cohorts.RData"))


# plot proportion of regions faceted by cancer and time in a barchart


Proportions_regions_cancer_cohorts <- Proportions_regions_cancer_cohorts  %>%
  mutate(`Region` = factor(`Region`, levels=c("Scotland", "Wales","England", "Northern Ireland"))) 

regions_cancer_plot <- 
  ggplot(Proportions_regions_cancer_cohorts, aes(x = reorder(Region, freq), y=freq, fill=Region)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Cancer~factor(Time, c("Before lockdown", "During lockdown", "After lockdown")),nrow=4, scale="free_y") +
  ggtitle("Proportion of cancer patients in each region of the UK diagnosed before, during and after lockdown") +
  ylab("Proportion") + xlab("Region")+
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 10), labels=function(y) paste0(y,"%")) +
  #  guides(fill = guide_legend(reverse = FALSE))+
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  coord_flip() 

ggsave(here("Results", db.name , "Regions", "Regions_cancer_plot.jpg"), regions_cancer_plot, dpi=600, scale = 1, width = 18, height = 9)

