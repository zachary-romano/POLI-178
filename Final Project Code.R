---
title: "Afghanistan Health Data Analysis"
output: html_document
author: Zachary Romano
date: Fall 2021
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Clear workspace
rm(list=ls(all=TRUE))

# Load libraries
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(qwraps2)
library(pander)
```

```{r loading data, include=FALSE}
data <- read_csv("Afghanistan Health Data.csv") 
```

```{r create intervention_type variable}
# Create new variable that specifies whether the row was pre- or post- U.S. intervention
data$intervention_type <- ifelse(data$TIME_PERIOD >= 2001, "Post-Intervention", "Pre-Intervention")

# Set levels for the new variable
data$intervention_type <- factor(data$intervention_type, levels = c("Pre-Intervention", "Post-Intervention"))
```


```{r subset data}
# Subset the dataset to look only at mortality rate data
mortality_rate <- data %>%
  filter(Indicator == "Mortality rate age 5-24")
```

```{r look at mean and changes in rate over time}
# Mean mortality rate
summary(as.numeric(mortality_rate$OBS_VALUE))

# Plot youth mortality rate over time
ggplot(data = mortality_rate, aes(x = TIME_PERIOD, y = as.numeric(OBS_VALUE), color = intervention_type)) +
  geom_line() +
  labs(x = "Year", 
       y = "Mortality rate (deaths per 1000 people aged 5-24)", 
       title = "Mortality rate (5-24 year olds) by year", 
       caption = "Data Source: UN IGME") +
  geom_point() +
  theme_bw()
```

```{r}
# Generate variable that gives that average mortality rate by intervention type
mean_mortality_rate <- mortality_rate %>%
  group_by(intervention_type) %>% 
  summarise(mean_mortality_rate = mean(as.numeric(OBS_VALUE), na.rm = TRUE))

ggplot(mean_mortality_rate, aes(x=intervention_type, y=mean_mortality_rate, fill=intervention_type)) +
  xlab("Intervention Type") +
  ylab("Average Mortality Rate") +
  geom_bar(stat="identity") + 
  labs(title = "Average Mortality Rate by Intervention Type") +
  theme_bw() +
  labs(fill = "Intervention Type") +
  geom_text(aes(label=mean_mortality_rate), vjust=2, color="black", size=3)
```

```{r look at the difference in mean and sd by intervention_type}
group_by(mortality_rate, intervention_type) %>%
  summarize(
    count = n(),
    mean = mean(as.numeric(OBS_VALUE), na.rm = TRUE),
    sd = sd(as.numeric(OBS_VALUE), na.rm = TRUE)
  )
```

```{r t-test}
# Subset data into two datasets: one for pre-intervention and one for post-intervention
pre_int <- subset(mortality_rate, mortality_rate$intervention_type == "Pre-Intervention")
post_int <- subset(mortality_rate, mortality_rate$intervention_type == "Post-Intervention")

# Save the mortality rate values in an array
pre_int_vals <- pre_int$OBS_VALUE
post_int_vals <- post_int$OBS_VALUE

# Run a t-test to see if there is a statistically significant difference in means
t.test(as.numeric(pre_int_vals), as.numeric(post_int_vals))
```

```{r t-test results table}
panderOptions("table.split.table", Inf) 
pander(t.test(as.numeric(pre_int_vals), as.numeric(post_int_vals)))
```

```{r regression plot}
# Plot youth mortality rate over time
ggplot(data = mortality_rate, aes(x = TIME_PERIOD, y = as.numeric(OBS_VALUE), color = intervention_type)) +
  geom_line() +
  labs(x = "Year", 
       y = "Mortality rate (deaths per 1000 people aged 5-24)", 
       title = "Mortality rate (5-24 year olds) by year", 
       caption = "Data Source: UN IGME") +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  theme_bw()
```

```{r regression results}
# Look at regression line for pre-intervention data
tab_model(lm(as.numeric(OBS_VALUE) ~ TIME_PERIOD, data = pre_int))

# Look at regression line for post-intervention data
tab_model(lm(as.numeric(OBS_VALUE) ~ TIME_PERIOD, data = post_int))
```