# CODE TO CREATE A FIGURE OF THE OBSERVATION PERIOD

library(readxl)
Lockdown_Periods_Dates <- read_excel("~/OPTIMA cancer and covid project/Analysis/Lockdown Periods Dates.xlsx")
View(Lockdown_Periods_Dates)

library(ggplot2)
library(dplyr)
library(here)

lockdown <- data.frame(periods = Lockdown_Periods_Dates$Periods, start = as.Date(Lockdown_Periods_Dates$Start_date), end = as.Date(Lockdown_Periods_Dates$End_date))
lockdown$lockdown <- 1

lockdown <- lockdown  %>%
  mutate(periods = factor(periods, levels=c("Pre-Lockdown", "Lockdown", "Post-First Lockdown", "Second Lockdown", 
                                            "Third Lockdown", "Easing of Restrictions", "Legal Restrictions Removed")) )

lockdown <- lockdown %>% rename("Lockdown Periods" = periods) 

start_date <- as.Date("2017-01-01") 

Observation_period_plot <- ggplot(lockdown, aes(x = start, y = lockdown, fill = `Lockdown Periods`)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 1), alpha = 1) +
  scale_fill_discrete() +
  theme_minimal() +
  labs(x = "Date", y = "Lockdown Period") +
  # scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="1 month", limits = as.Date(c("2017-01-01", "2022-07-01")), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
        axis.title.x = element_text(size=12),
        axis.text.y = element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title.y=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

Observation_period_plot

ggsave(here("Results", db.name, "Observation_period_plot.jpg"), Observation_period_plot, dpi=600, scale = 1,  width = 12, height = 10)

