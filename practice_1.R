rm(list=ls(all=TRUE))
require(tidyverse)
setwd("~/Downloads/School/POLI 178")
load("acled.Rdata")

## Recursion 
# One of the most useful parts of R is that we can write and re-write objects. 
# This is the key to containers in loops. 


# To see it in action, let's remember that we can make vectors with the function c() :
x <- c("start", "start", "start")

x

# Now we can add elements to x by writing a function that calls x, then re-writes x:
x <- c(x, "new")
x

# If we run this again, x grows again:
x <- c(x, "new")
x

# We can do the same thing with dataframes. 
# In the case of a dataframe, we want to add rows one at a time using rbind
 
x_tib <- data.frame()
x_tib # An empty tibble. 

x_tib <- rbind(x_tib,x)
x_tib # Notice that we are treating x as one row. 

# We can do this again and again adding new rows:
x_tib <- rbind(x_tib,x)
x_tib <- rbind(x_tib,x)


#### This is useful if we create lots of new values in a loop and want to save them all as a row: 

country <- rep(c('Afghanistan', 'Brazil', 'China', 'Australia', 'France', 'Fiji'), each=2)
year <- rep(1999:2000, 6)
mil_exp <-  c(678, 756, 7563, 4389, 1003456, 1306481, 4563,5431, NA, 9342, NA, 134)
population <- c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583, 21435654,21012345, 109034567,100203456, 4542, 44534)

dat <- data.frame(country, year, mil_exp, population)

##############################
## The following loop simply re-creates the dataframe. 
## The point is to illustrate how we can use rbind as a container.
#############################

container <- data.frame() # an empty dataframe:

for(i in 1:nrow(dat)) {
a_country <- dat$country[i]  # return's the i'th country 
a_year <- dat$year[i] 
a_mil_exp <- dat$mil_exp[i]
result <- c(a_country,a_year,a_mil_exp) # Creates a vector of our three things.
container <- rbind(container, result)  # binds our row to the tibble we had.
}

colnames(container) <- c("county", "year", "mil_exp")
container

## NOTICE: We could have achieved the same result by creating 3 vectors 
#and binding them together afterwards

cont_1 <- vector()
cont_2 <- vector()
cont_3 <- vector()

for(i in 1:nrow(dat)) {
  cont_1 <- c(cont_1, dat$country[i])  # adds the i'th country to our first container 
  cont_2 <- c(cont_2, dat$year[i])  # adds the i'th country to our first container 
  cont_3 <- c(cont_3, dat$mil_exp[i])   
}

result <- tibble(country=cont_1, year=cont_2, mil_exp = cont_3)
result




#############################
##########
## Filter, summarize, re-shape.
####################

# Some practice with filters. 
# Summarizing data using count.
# Reshaping data using spread
# Creating new variables using mutate

### Let's say we want to compare  Somalia and Sudan in 2019. We can do it using Filter. 
 

dat <- acled %>% 
  filter(country=="Somalia" |country=="Sudan" ) %>%   # Notice, the " | " allows us to do a conditional statement. 
  filter(year ==2019)

## Another way to go is to specify our countries of interest: 
 acled %>% 
  filter(country%in%c("Somalia", "Sudan")) %>%   
  filter(year ==2019) 

#We can check what this is doing by running:
acled$country%in%c("Somalia", "Sudan")

#Notice that this output is the same length as the number of rows in ACLED.
# It moves through each observation in acled$country asking "Is this element also in c("Somalia", "Sudan")


## Also notice that we can achieve the same thing using one filter: 
acled %>% 
  filter(country%in%c("Somalia", "Sudan") & year ==2019) 


########### 
# Do these countries have different kinds of events? 
#### 


#To answer this question we need to count the events in them: 

dat %>% 
  count(country, event_type)

# Hmmm.... our goal is to easily compare the event types across coutnries. BUT the data is arranged by country. Can you think of a way to re-arrange it so that it is arranged by event_type?

dat %>% 
  count(country, event_type) %>% 
  arrange(event_type) 


### It's better, but it is still not great for a comparison. 
# How about we create a dataframe

### Let's change the data shape so that each event type is a row and each column counts the country. 

dat %>% 
  count(country, event_type) %>% 
  spread(country, n)

### What just happened? 
# Notice, the count function is the same as above. So the first step is to count up the event-types.
# The last line of the pipe re-shapes the data from long format, to wide format. 
    # Effectively, we are changing the unit of analysis from country-event type, to event type. 
    # Where does the "n" come from in spread? 
      # n is the column name that count spits out in counting the events. 



### A better comparison helps us see the difference in the event-types. We can do this by creating one column that subtracts Somalia event count from Sudans

dat %>% 
  count(country, event_type) %>% 
  spread(country, n) %>% 
  mutate(difference = Somalia - Sudan) %>%  # This creates a new variable "difference"
  arrange(difference)  # This orders them


################################
##  Your turn
################################

# How has conflict changed in Somalia over time in different administrative districts? 
# The administrative districts are recorded in column `acled$admin1`

# Create a dataframe where each column is a a year, and each row is an administrative district. Then count the total number of events. 
# It should look something like this:
#admin1        `1997` `1998` `1999` `2000` `2001` `2002` `2003` `2004` `2005` `2006` `2007` `2008` `2009` `2010` `2011` `2012` `2013` `2014` `2015` `2016` `2017` `2018` `2019`
#<chr>          <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
#  1 Awdal             NA     NA      2     NA     NA      1     NA     NA     NA      3      2      2      5      5      2      7     31     30     48     36     17     14     12
#2 Bakool             4      1      3      1      2      1     NA      7     19     11     12     16     19     28     12     50     99    151    122    102     83     51     58
#3 Banadir            5     11     17     29     28     40    322    161    103    115    788    651    294    680    509    667    973    744    494    541    689    780    629
#4 Bari              NA      1      1      5      1      3      3     12     12     21     24     35     23    104     74     73    138     69     79    139    181    168     96
#5 Bay                2      7     23      4      9     34     14     46     25     91     48    170     22     22     22    185    251    267    251    178    211    185    165
#6 Galgaduud         NA     NA      1      7     NA      3      1     14     18     26     17     53     22     90     71     96     66     89     76     98     87     45     41
#  ... 

acled %>%
  filter(country == "Somalia") %>%
  count(admin1, year) %>%
  spread(year, n)


###############################
# Now do the same thing, but with a fatality count: 
# That is, each entry should be the sum of the number fatalities in each administrative unit in each year. 
##################################


#`summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
# A tibble: 18 x 24
#admin1        `1997` `1998` `1999` `2000` `2001` `2002` `2003` `2004` `2005` `2006` `2007` `2008` `2009` `2010` `2011` `2012` `2013` `2014` `2015` `2016` `2017` `2018` `2019`
#<chr>          <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#  1 Awdal             NA     NA      1     NA     NA      5     NA     NA     NA      0      0      1      6      0      0      2      4      0      4      2      2      0     10
#2 Bakool             0      0      0      0      0      0     NA      3     51     12      5      7     24     75      7    143    171    501    312    259    166     72     52
#3 Banadir            6      4     25     23     26     26    663    596     85    508   1476   1133    600   2413    970    525    711    598    557    696   1445    976    738
#4 Bari              NA      0      0      6      0      0      8     25      0     18     19     40     19    117     99    107     80     73    157    147    356    286     71
#5 Bay                1      0     24      0      0     32     24     89     82    227     60    170     18     17     12    298    346    442    532    447    529    378    309
#6 Galgaduud         NA     NA      0      1     NA      0      2     35     51     95     25     80     94    334    168    144     51    187    220    310    156    110     33

acled %>%
  filter(country == "Somalia" & fatalities > 0) %>%
  count(admin1, year) %>% 
  spread(year, n)

###############
# NOTICE: NAs are introduced because spread forces 1 entry for every admin-year. When there are no events recorded, spread enters an NA. 

## A tip: You can use the option replace_na(0) inside of spread. 
#That should get you this:
#  admin1        `1997` `1998` `1999` `2000` `2001` `2002` `2003` `2004` `2005` `2006` `2007` `2008` `2009` `2010` `2011` `2012` `2013` `2014` `2015` `2016` `2017` `2018` `2019`
#<chr>          <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#  1 Awdal              0      0      1      0      0      5      0      0      0      0      0      1      6      0      0      2      4      0      4      2      2      0     10
#2 Bakool             0      0      0      0      0      0      0      3     51     12      5      7     24     75      7    143    171    501    312    259    166     72     52
#3 Banadir            6      4     25     23     26     26    663    596     85    508   1476   1133    600   2413    970    525    711    598    557    696   1445    976    738
#4 Bari               0      0      0      6      0      0      8     25      0     18     19     40     19    117     99    107     80     73    157    147    356    286     71
#5 Bay                1      0     24      0      0     32     24     89     82    227     60    170     18     17     12    298    346    442    532    447    529    378    309
#6 Galgaduud          0      0      0      1      0      0      2     35     51     95     25     80     94    334    168    144     51    187    220    310    156    110     33




