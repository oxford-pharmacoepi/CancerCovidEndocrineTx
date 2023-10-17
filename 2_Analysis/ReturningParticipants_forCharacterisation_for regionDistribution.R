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

C1 <- breast_prostate_regions %>% filter(cohort_definition_id==1)%>% mutate(freq = (n/sum(n))*100) 
C2 <- breast_prostate_regions %>% filter(cohort_definition_id==2)%>% mutate(freq = (n/sum(n))*100) 

Proportions_regions_breast_prostate <- bind_rows(C1,C2)

# LABEL THE COHORTS FOR VISUALISATION
Proportions_regions_breast_prostate <-  Proportions_regions_breast_prostate %>% mutate(Cancer = case_when(cohort_definition_id ==1 ~ "Breast",
                                                                                                        (cohort_definition_id ==2) ~ "Prostate"))


# RENAME REGIONS_COLLAPSED
Proportions_regions_breast_prostate <- Proportions_regions_breast_prostate %>% rename(Region = "region_collapsed") 

write.csv(Proportions_regions_breast_prostate, file=here::here("Results", db.name, "Regions", "Proportions_regions_breast_prostate.csv"))
save(Proportions_regions_breast_prostate, file=here::here("Results", db.name, "Regions", "Proportions_regions_breast_prostate.RData"))


# plot proportion of regions faceted by cancer and time in a barchart


Proportions_regions_cancer_cohorts <- Proportions_regions_cancer_cohorts  %>%
  mutate(`Region` = factor(`Region`, levels=c("Scotland", "Wales","England", "Northern Ireland"))) 

regions_breast_prostate_plot <- 
  ggplot(Proportions_regions_breast_prostate, aes(x = reorder(Region, freq), y=freq, fill=Region)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Cancer, scale="free_y") +
  ggtitle("Proportion of breast and prostate cancer patients in each region of the UK") +
  ylab("Proportion") + xlab("Region")+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), labels=function(y) paste0(y,"%")) +
  #  guides(fill = guide_legend(reverse = FALSE))+
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  coord_flip() 

ggsave(here("Results", db.name , "Regions", "regions_breast_prostate_plot.jpg"), regions_breast_prostate_plot, dpi=600, scale = 1, width = 18, height = 9)

