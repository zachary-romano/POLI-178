# Zachary Romano
# POLI 178
# Homework 3
# 11 October 2021
################################################################################

# Clear environment
rm(list=ls(all=TRUE))

# Load libraries
require(tidyverse)
require(gapminder)

# Load gapminder data and look at it
gapminder <- gapminder
head(gapminder)

glimpse(gapminder)
slice(gapminder)

###### TASK 1 ######

# Look at summary statistics for each column
summary(gapminder)

# What is the temporal coverage?
unique(gapminder$year)

# This dataset contains data from 1952 to 2007, and the data is recorded every 5 years.

# What continents does it cover?
unique(gapminder$continent)

# Africa, Asia, the Americas, Europe, and Oceania are covered in this dataset.

# How is the population and life expectancy data dispersed (mean, min, max, variance)?
summary(gapminder$pop)
var(gapminder$pop)

# The population statistics within this dataset range from 60,011 to 131,863,096. 
# The average population is 29,601,212, but the variance is very high (~1.13e+16).

summary(gapminder$lifeExp)
var(gapminder$lifeExp)

# The life expectancy within this dataset ranges from 23.6 to 82.6 years old.
# The average life expectancy over country and time is 59.47, and the variance is equal to 166.85.

###### TASK 2 ######

# We are going to reduce the columns. Using select, print:

# a. only the lifeExp and population columns
gapminder %>%
  select(lifeExp, pop)

# b. only the columns where the header contains the letter “c”
gapminder %>%
  select(contains("c"))

# c. all columns except lifeExp.
gapminder %>%
  select(-lifeExp)

###### TASK 3 ######

# Using one pipe, create a second data frame afr_gapminder that is only African countries post-1990.
afr_gapminder <- gapminder %>%
  filter(continent == "Africa" & year > 1990)

# Look at new data frame
afr_gapminder

###### TASK 4 ######

# Using 1 pipe, create a tibble entry_obs that returns only one row for each country that is the first year that country enters the dataset.
entry_obs <- gapminder %>%
  arrange(country, year) %>%
  filter(year == min(year))     ### I tried to use only one pipe to complete for this problem, but could not figure out how to do it.

# Print the sum of the population column from entry_obs and put that result as a comment into your code.
sum(entry_obs$pop)

###### TASK 5 ######

# For the tibble gapminder create two new variables

# a. A variable for the population five years prior.
gapminder <- gapminder %>%
  group_by(country) %>%
  mutate(pop_5_years_ago = lag(pop))

head(gapminder)

# b. A variable that measures the change in population in one country over a 5 year period.
gapminder <- gapminder %>%
  group_by(country) %>%
  mutate(pop_change = pop - pop_5_years_ago)

head(gapminder)

###### TASK 6 ######

# Create a new tibble long_gap where life expectancy, population and gdpPer cap are all in one column. 
# There is also a second column that explains what each variable type is.

long_gap <- gapminder %>%
  pivot_longer(cols = c(lifeExp, pop, gdpPercap), names_to = "variable_type")

head(long_gap)


