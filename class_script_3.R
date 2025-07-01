
require(tidyverse)
require(gapminder)
require(countrycode) # This is new, we'll talk about it in a minute. 
setwd("~/Downloads/School/POLI 178")

#This lecture we will illustrate the difficulties and challenges of data wrangling using two datasets. 

#The first dataset is `gapminder` which we examined in our homework. 

gapminder <- gapminder


#The second dataset is the **ACLED** dataset. 

load("~/Downloads/School/POLI 178/acled.Rdata")

acled
glimpse(acled)


#################
# Our Goal: 
########################

#Create a dataframe for analysis that combines the demographic data in gapminder with the conflict data in ACLED.

#This is what we would need to do if we want to examine the affect of life expectancy on political dissent. Or the effect of population growth on government-led repression. 

#If we were going to focus on a specific question, we would be more careful in the ACLED data that we focused on. For today, I just want to emphasize the practical difficulties with wrangling in general. 


#################
# Summary  ACLED
################
#Since ACLED is new to us, we need to answer some basic questions:
  
#  + What is the unit of analysis? 
#  + It's an event. The event occurs in a location and at a time. 

#  + What are the ID keys?

#  + Location identifiers

#  + Temporal identifiers


### Summary stats:
# We want to:
#  a. Learning about the events that ACLED contains. 
#  b. Learning more about the data structure so that we can integrate with gapminder. 

### a. Events in ACLED: 
# We have event types, and fatalities associated with event types. 


table(acled$event_type)  # main category
table(acled$sub_event_type) # sub-category
summary(acled$fatalities) # How frequently, do events end up killing someone?

#Hmmm... Not many events are fatal. If our interest is in violent events, should we keep all event-types? Let's dig a little more?
  
acled %>% filter(fatalities > 0) %>% 
  select(event_type) %>% table() 

acled %>% filter(fatalities > 0) %>%
  select(sub_event_type) %>% table() 

#Missing data.
acled %>% 
  complete.cases() %>% 
  table()


acled %>% select(event_type, year, fatalities) %>%
  complete.cases() %>% table()


### b.ACLED's structure: 
#  We need to connect our data  with gapminder. 

#Remember, each row in our data is a unit of observation. So we need to decide on a single unit to put ACLED and gapminder into. 

#The unit of analysis we choose should primarly be driven by our theory. However, we are limited by our data! We need to see what's possible. 

#In our homework we learned that gapminder covered ALL countries in 5-year intervals.  Does our ACLED data cover the same countries? 

unique(acled$country)


#Nope. (Note: ACLED has more countries. We started with the AFRICA sample.)

#Does it cover the same time span as gapminder? 

sort(unique(acled$year))



### Other issues:

#ACLED records MANY events for the same country in the same year. So there is not one observation for each country-year. 

#The country names in ACLED do not match the country-names in gapminder... 


## Our goal given the limitations.

#  1. Create a country-year data set ONLY for the common years and countries. 
#  2. Create one variable that counts the number of events in each country-year 
#  3. Create one variable that counts the number of FATAL events in each country-year 
#  4. Create one variable that sums the number of fatalities in each country-year. 
#  5. Sum the number of fatalities from civilian led violence for each country year


acled_cy <- acled %>% # First, lets create a dataset of country-year with all events conted. 
  count(iso,year) %>% 
  rename("event_count" = "n")     # NOTICE, it removes all the rows other than iso, year and our count... 

acled_cy <- acled %>% 
  filter(fatalities >0) %>% 
  count(iso,year) %>% 
  rename("fat_event_count" = "n") %>% 
  right_join(acled_cy, by=c("iso", "year"))  #This recursively joins what we had, with our new count. 

acled_cy <- acled %>% group_by(iso, year) %>% 
  summarize(fatalities = sum(fatalities)) %>% left_join(acled_cy, by=c("iso", "year"))

acled_cy <- acled %>%
  filter(event_type == "Protests" | event_type == "Riots") %>%
  group_by(iso, year) %>%
  summarize(fatalities_civ = sum(fatalities)) %>%
  left_join(acled_cy, by=c("iso", "year"))

acled_cy


#Now gapminder. We want to focus on Africa we can do that using the continent code. 


gapminder_af <- gapminder %>% filter(continent=="Africa")


# Countrycode. 

#When R reads data, it needs exact names. It does not know that "DRC" and "Congo" are the same country. 

#We need our keys to be an exact match! 

#Fortunately, there is a package that helps us: `countrycode`
#`countrycode` relies on a dataframe that matches includes hundreds of common ways of writing out country names. 

countrycode::codelist

#`countrycode` includes a function that translates one coding type to another. 

#countrycode([an input vector], origin = '[coding scheme]', destination = '[coding scheme]')

#We can back out the iso code to gapminder using: 

sample(gapminder_af$country)  # These are country NAMES. 

countrycode(gapminder_af$country, origin = 'country.name', destination = 'iso3n')

gapminder_af$iso <-  countrycode(gapminder_af$country, origin = 'country.name', destination = 'iso3n')



## Joining the data together.

gap_acled <- left_join(gapminder_af,acled_cy, by = c('iso', "year"))
gap_acled


#What do you notice about the NAs? 

gap_acled <- gap_acled %>% filter(year %in% acled_cy$year)
gap_acled


#For emphasis, let's consider a different way to join the data:
  

gap_acled2 <- inner_join(gapminder_af,acled_cy, by = c('iso', "year"))

nrow(gap_acled)
nrow(gap_acled2)


#Are these the same? 
 
#  To be clear, there is a lot of NA's in in our event_count / fat_event_count lists.  How should we treat these?  

#What generated the NA's in this case?  It occurs because there is an observation for Benin in 1997. But there was no fatal ACLED event in that year. So we get an NA.

#In my opinion, this should really be a '0'.  So we ought to convert it. 


gap_acled$fat_event_count[which(is.na(gap_acled$fat_event_count )==T)] <- 0
gap_acled$event_count[which(is.na(gap_acled$event_count)==T)] <- 0
gap_acled$fatalities [which(is.na(gap_acled$fatalities)==T)] <- 0


#Now we'll save it for next time. 


#setwd("...") # Make sure you set your wd where you want to save the file... 

#write_csv(gap_acled, 'gap_acled.csv')




# Summary:

#Our wranling balances two issues:

#  + Theory: What is the data structure we need to answer our question?
 # + Data: What is the data we have? 
  
#Summary statistics are key:

#  + Helps identify commonalities across datasets.
#  + Identifies coding choices made by the data creators. 
#  + Along the way, computing summary statistics illuminates mistakes based on the choices we make. 

#Countrycode is useful for international relations data. 

