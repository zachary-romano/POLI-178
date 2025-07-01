#Old favourites
require(tidyverse)
require(ggplot2)
require(ggthemes)

# APIS we'll use in class
require(trendyy)
require(gtrendsR)
require(wbstats)

# Fun with dates.
require(lubridate)


########################
##  Plan:

#- APIs.
#- Writing Functions
#- Dates. 


###############
#APIS
#####################

# APIs allows you to query databases and download structured data directly into R.   
        
#There are R APIs for:
#- World Bank, State Department, EU
#- Twitter 
#- Google search terms. 
#- Qualtrics 
#- New York Times
        
        
## APIs Are written by the data owner. 
        
#They decide: 
# - what data you are allowed to download. 
# - how you download it. 
        

########
## Google API.  
#########
#Allows you to download the frequencey of search terms by location. 
      
my_search_terms <- c("Biden","Trump")  # The words we want to know about. 

trends <- trendy(search_terms = my_search_terms, 
                 from = "2020-09-01",to="2020-09-30") # Googles' function. 
  

### Let's look at the results. 
          
dat_trends <- get_interest(trends)
dat_trends

### A plot
        dat_trends %>%
          ggplot(aes(date,hits,color=keyword)) +
          geom_line() +  geom_point() +
          ggthemes::scale_color_fivethirtyeight() +  # We'll add a cool theme. 
          ggthemes::theme_fivethirtyeight()

        
### We can also look at countries or EVEN city searches.  

geo_codes <- as_tibble(gtrendsR::countries) ## This code gets us the list of country-codes. 
geo_codes  


### China Plot
trends_china <- trendy(search_terms = my_search_terms,
                      from = "2004-01-01",to="2020-09-30",
                               geo="CN")
        trends_china <- get_interest(trends_china)
        trends_china %>%  ggplot(aes(date,hits,color=keyword)) +  geom_line() +
          ggthemes::scale_color_fivethirtyeight() + ggthemes::theme_fivethirtyeight()

###  Limitations: 
#- Normalized search terms means that content is relative to the words searched. We can't back out search volume.

#- Google limits us
#   + Can only search for 5 keywords at a time.
#   + Can only request 1000 queries a day.

#- Choosing the correct keywords can be difficult. Language matters.
 
### World Bank API.  


# Look up indicators related to GDP.
gdp_ind <- wb_search(pattern = "gdp")

gdp_ind 


wb_dat <- wb(indicator = "6.0.GDP_current", # Use the indicator id
             country = "countries_only", # Avoid regional designations
             startdate =2000, enddate=2005)
wb_dat <- as_tibble(wb_dat)



wb_dat %>%
  mutate(date = as.numeric(date),
         ln_gdp = log(value)) %>% 
  ggplot(aes(date,ln_gdp)) +
  geom_smooth(method="loess",color="steelblue",fill="steelblue") +
  ggthemes::theme_economist()


###  Let's look at the results. 
        
wb_dat %>%
        mutate(date = as.numeric(date),
             ln_gdp = log(value)) %>% 
        ggplot(aes(date,ln_gdp, color=country)) +
        geom_line() +
          ggthemes::theme_economist()

### Other APIs
#- `rtweet` R client for accessing Twitter's REST and stream APIs.
  #+  Need an authorization key to download data. Can take awhile to get.
#-  `WikipediR` 
#- `qualtRics`
#  + Allows you to download your surveys directly into R!





### What's a 'key'?
#Some APIs require you to have a user name and password. 
#e.g. before you log into `qualtRics` you must run
library(qualtRics)
qualtrics_api_credentials(api_key = "<YOUR-QUALTRICS_API_KEY>", 
base_url = "<YOUR-QUALTRICS_BASE_URL>",
install = TRUE)


#########
#  Writing Functions
#######################        
## Writing Functions
          
          
# We can write one as follows:

# Basic Set Up
    my_function = function(x,y) # Arguments broken up by commas
        { # Brackets that house the code 
 # Some code to execute 
          z = x+y
 return(z) # Return a data value
        }
        
my_function(5,7)


#########
# Dates
#####################        
#`R` has a specific `Date` class. We will use the function `as.Date()` to coerce a relevant string into a date class.
        
 str <- "2006-04-30"
 class(str)
 date_str <- as.Date(str)
 class(date_str)
 
#Objects of class date have some nice properties, that makes analysis and manipulation easy.
date_str
date_str + 30 # date in 30 days
date_str - 3000 # date 300 days ago.

#This also allows us to look at the distance between two dates.
date1 = as.Date("2015-06-07")
date2 = as.Date("2013-02-14")
        
date1
date2
date1-date2

## Formatting Dates
          
#That said, dates come in many different formats. To let `R` know that a specific string is a date, we have to tell it the **date format**.
example <- "February 3, 1987"
as.Date(example)

as.Date(example, format = "%B %d, %Y")

  
#**Formatting dates** is requires that we articulate to `R`via special syntax what each date feature is. In a string (i.e. using " "), we specify the exact pattern of the date with **_all appropriate punctuation and spacing_**.
        
as.Date("Friday March 13, 2009","%A %B %d, %Y")

as.Date("11/13/14","%m/%d/%y")


as.Date("7th of May 2000","%dth of %B %Y")

# How would we convert this date: `03Feb2009`?

as.Date("03Feb2009","%d%b%Y")


## Things to think about:        
# How would we convert this date: `01/10/02`?
# Tricky... Which is the month? Year? Day?
as.Date("01/10/02","%d/%m/%y")
as.Date("01/10/02","%y/%m/%d")
as.Date("01/10/02","%m/%y/%d")

##############
## Lubridate
################

our_date = as.Date("1990-05-03")
our_date

year(our_date)

month(our_date)
day(our_date)

#Quick parsing features
ymd("1990/05/03")

ydm("1990/03/05")

dmy("03/05/1990")

# Gather qualitative labels
wday(our_date,label=T)

wday(our_date+5,label=T)


##################
#More complex expressions of time.
################
our_date2 <- "2009-05-04 05:11:33"
ymd_hms(our_date2)
          
#Specify time zone
tt <- ymd_hms(our_date2,tz = "EST")
tt
          
#Convert time zone.
with_tz(tt,tzone = "America/Boise")

## Rounding dates 
our_date
round_date(our_date,unit = "week")

round_date(our_date,unit = "month")
round_date(our_date,unit = "year")
floor_date(tt,unit = "hour")

ceiling_date(tt,unit = "minute")

###
#Duration
dyears(3)
dweeks(3)

# How many seconds of your life are you spending in this class?
dhours(.837) 



setwd("C:/Users/Michael/Dropbox/Sylibus/analytics_intel/data/")
load("acled.Rdata")

acled %>% select(1:10)

as.Date(acled$event_date,"%d %B %Y") 

as.Date(acled$event_date,"%d %B %Y") %>% 
  round_date(unit = "month")




###################
# Apply functions
#######################
dat <- tibble(a =  5:30, b = 105:130)

apply(dat,1, mean) 
apply(dat,2, mean) 


