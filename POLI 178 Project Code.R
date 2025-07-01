rm(list=ls(all=TRUE))

library(tidyverse)
library(ggplot2)

data <- read_csv("Afghanistan Health Data.csv")

unique(data$Indicator)

infant_mortality <- data %>%
  filter(Indicator == "Infant mortality rate")

mean(as.numeric(infant_mortality$OBS_VALUE))

ggplot(data = infant_mortality, aes(x = TIME_PERIOD, y = as.numeric(OBS_VALUE), group = Sex, color = Sex)) +
  geom_line() +
  labs(x = "Year", y = "Infant mortality rate (Deaths per 1000 live births)", title = "Infant mortality rate over time") +
  geom_point() +
  theme_bw()

limited_drinking_water <- data %>%
  filter(Sex == "Total" & Indicator == "Proportion of population using limited drinking water services")

limited_drinking_water

ggplot(data = limited_drinking_water, aes(x = TIME_PERIOD, y = as.numeric(OBS_VALUE), group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Proportion of population using limited drinking water services", title = "Proportion of population using limited drinking water services over time") +
  geom_point() +
  theme_bw()

youth_HIV <- data %>%
  filter(Indicator == "Estimated number of adolescents and young people (aged 15-24 years) living with HIV")

ggplot(data = youth_HIV, aes(x = TIME_PERIOD, y = as.numeric(OBS_VALUE), group = Sex, color = Sex)) +
  geom_line() +
  labs(x = "Year", y = "Estimated number of adolescents and young people (aged 15-24 years) living with HIV", title = "Estimated number of adolescents and young people (aged 15-24 years) living with HIV by year") +
  geom_point() +
  theme_bw()

mortality_rate <- data %>%
  filter(Indicator == "Mortality rate age 5-24")

mean(as.numeric(mortality_rate$OBS_VALUE))

ggplot(data = mortality_rate, aes(x = TIME_PERIOD, y = as.numeric(OBS_VALUE), group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Mortality rate (deaths per 1000 people aged 5-24", title = "Mortality rate (5-24 year olds) by year") +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm")
