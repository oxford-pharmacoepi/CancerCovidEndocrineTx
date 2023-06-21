# Locating the location of the patients across time. CPRD has changed across the
# last few years, with more practices moving over to AURUM

# Connect to the database and then view the location table


# load r packages

library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(tidyr)
library(CDMConnector)
library(RPostgres)


# table names instantiated in cdm with cancer diagnoses
# "nb_cancer_covid_cancers_3_time_periods"

outcome_table_stem <- "nb_cancer_covid" # this will form the start of the table name where the cohorts are instantiated
# table names----
outcome_table_name_1 <- paste0(outcome_table_stem,"_cancers_before_after_lockdown") # this is the four cancers
outcome_table_name_2 <- paste0(outcome_table_stem,"_cancers_3_time_periods") # this is the four cancers before, during and after lockdown
outcome_table_name_3 <- paste0(outcome_table_stem,"_denominator_3_time_periods") # this is the denominator before, during and after lockdown

db.name<-"CPRDGold_202201"


# connect to database
user        <-  Sys.getenv("DB_USER")
password    <-  Sys.getenv("DB_PASSWORD")
port        <-  Sys.getenv("DB_PORT") 
host        <-  Sys.getenv("DB_HOST") 
server_dbi  <-  Sys.getenv("DB_SERVER_DBI_cdm_gold_202201")
server      <-  Sys.getenv("DB_SERVER_cdm_gold_202201")

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
# see here for details: https://odyosg.github.io/CDMConnector/articles/DBI_connection_examples.html
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"public"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-"results"


# create cdm reference for 1st ever cancer diagnoses---- 
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)
#cohort_tables = "nb_cancer_covid_cancers_3_time_periods")
#cohort_tables = "nb_cancer_covid_denominator_3_time_periods")

# count n in each cohort definition
cdm$nb_cancer_covid_cancers_3_time_periods %>% group_by(cohort_definition_id) %>% tally() %>% print(n=Inf)
cdm$nb_cancer_covid_denominator_3_time_periods %>% group_by(cohort_definition_id) %>% tally() %>% print(n=Inf)


cdm$location %>% head()

# Group by location source value

cdm$location %>% group_by(location_source_value) %>% distinct() %>% tally() %>% collect()


# ============================================================================ #
#       Get region distribution for cancer cohorts in 3 time period            #
#          (make sure the correct cohort table is read into the cdm)           #
#                 cdm$nb_cancer_covid_cancers_3_time_periods                   #
# ============================================================================ #



region_distribution_cancer_cohorts <- cdm$nb_cancer_covid_cancers_3_time_periods %>% 
  rename("person_id" = subject_id) %>%
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  rename(subject_id = person_id) %>%
  group_by(cohort_definition_id, region) %>% 
  tally() %>% 
  arrange(cohort_definition_id) %>%
  collect()

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


# create a tibble of all 12 regions to later join with all regions so that you can populate cells with NA when missing rather than omitting the row
region_names <- tibble::as_tibble_col(c("East of England", "East Midlands", "London", "North East", "North West", "Northern Ireland", "Scotland", "South Central", 
                                        "South East Coast", "South West", "West Midlands", "Yorkshire  & The Humber"), column_name = "region")

C1 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==1)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 1)
C2 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==2)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 2)
C3 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==3)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 3)
C4 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==4)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 4)
C5 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==5)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 5)
C6 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==6)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 6)
C7 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==7)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 7)
C8 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==8)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 8)
C9 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==9)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 9)
C10 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==10)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 10)
C11 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==11)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 11)
C12 <- region_distribution_cancer_cohorts %>% filter(cohort_definition_id==12)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 12)


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

write.csv(Proportions_regions_cancer_cohorts, file=here::here("Results", db.name, "Regions", "Proportions_regions_cancer_cohorts.csv"))
save(Proportions_regions_cancer_cohorts, file=here::here("Results", db.name, "Regions", "Proportions_regions_cancer_cohorts.RData"))


# plot proportion of regions faceted by cancer and time in a barchart

regions_cancer_plot <- 
  ggplot(Proportions_regions_cancer_cohorts, aes(x = reorder(region, freq), y=freq, fill=region)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Cancer~factor(Time, c("Before lockdown", "During lockdown", "After lockdown")),nrow=4, scale="free_y") +
  ggtitle("Proportion of cancer patients in each region of the UK diagnosed before, during and after lockdown") +
  ylab("Proportion") + xlab("Region")+
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 10), labels=function(y) paste0(y,"%")) +
  coord_flip() 

ggsave(here("Results", db.name , "Regions", "Regions_cancer_plot.jpg"), regions_cancer_plot, dpi=600, scale = 1, width = 18, height = 9)




# ============================================================================ #
#       Get region distribution for denominator pops in 3 time period          #
#          (make sure the correct cohort table is read into the cdm)           #
#                 cdm$nb_cancer_covid_denominator_3_time_periods               #
# ============================================================================ #



region_distribution_denominator <- cdm$nb_cancer_covid_denominator_3_time_periods %>% 
  rename("person_id" = subject_id) %>%
  left_join(cdm$person %>% select(person_id, care_site_id), by = "person_id") %>% 
  left_join(cdm$care_site %>% select(care_site_id, location_id), by = "care_site_id") %>%
  left_join(cdm$location %>% select(location_id, region = location_source_value), by = "location_id") %>%
  select(-c(care_site_id, location_id)) %>%
  rename(subject_id = person_id) %>%
  group_by(cohort_definition_id, region) %>% 
  tally() %>% 
  arrange(cohort_definition_id) %>%
  collect()

write.csv(region_distribution_denominator, file=here::here("Results", db.name, "Regions", "region_distribution_denominator.csv"))
save(region_distribution_denominator, file=here::here("Results", db.name, "Regions", "region_distribution_denominator.RData"))

# CREATE PROPORTIONS COLUMN IN THE TABLE SPLIT BY COHORT AND REGION


# create a tibble of all 12 regions to later join with all regions so that you can populate cells with NA when missing rather than omitting the row
region_names <- tibble::as_tibble_col(c("East of England", "East Midlands", "London", "North East", "North West", "Northern Ireland", "Scotland", "South Central", 
                                        "South East Coast", "South West", "West Midlands", "Yorkshire  & The Humber"), column_name = "region")

D1 <- region_distribution_denominator %>% filter(cohort_definition_id==1)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 1)
D2 <- region_distribution_denominator %>% filter(cohort_definition_id==2)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 2)
D3 <- region_distribution_denominator %>% filter(cohort_definition_id==3)%>% mutate(freq = (n/sum(n))*100) %>% right_join(region_names) %>% mutate(cohort_definition_id = 3)


Proportions_regions_denominator <- bind_rows(D1,D2,D3)

# LABEL THE COHORTS FOR VISUALISATION
Proportions_regions_denominator <-  Proportions_regions_denominator %>% mutate(Time = case_when(cohort_definition_id == 1 ~ "Before lockdown",
                                                                                                cohort_definition_id == 2 ~ "During lockdown",
                                                                                                cohort_definition_id == 3 ~ "After lockdown"))


write.csv(Proportions_regions_denominator, file=here::here("Results", db.name, "Regions", "Proportions_regions_denominator.csv"))
save(Proportions_regions_denominator, file=here::here("Results", db.name, "Regions", "Proportions_regions_denominator.RData"))


# plot proportion of regions faceted by time in a bar chart

regions_denominator_plot <- 
  ggplot(Proportions_regions_denominator, aes(x = reorder(region, freq), y=freq, fill=region)) + 
  geom_bar(stat="identity") +
  facet_wrap(~factor(Time, c("Before lockdown", "During lockdown", "After lockdown")),nrow=4, scale="free_y") +
  ggtitle("Proportion of population in each region of the UK observed in CPRD before, during and after lockdown") +
  ylab("Proportion") + xlab("Region")+
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 55, 10), labels=function(y) paste0(y,"%")) +
  coord_flip() 

ggsave(here("Results", db.name , "Regions", "Regions_denominator_plot.jpg"), regions_denominator_plot, dpi=600, scale = 1, width = 18, height = 9)



# test - this might be wrong
region_test <- cdm$person %>% select(person_id, care_site_id) %>% left_join(cdm$care_site %>% select(care_site_id, location_id)) %>% left_join(cdm$location %>% select(location_id, location_source_value))
region_test