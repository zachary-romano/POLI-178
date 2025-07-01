rm(list = ls(all=TRUE))
require(tidyverse)
require(lubridate)

tab <- tribble(~country, ~year, ~cases, ~population,
                'Afghanistan',  1999,    745,   19987071,
                'Afghanistan',  2000,   2666,   20595360,
                'Brazil'     ,  1999,  37737,  172006362,
                'Brazil'     ,  2000,  80488,  174504898,
                'China'      ,  1999, 212258, 1272915272,
                'China'      ,  2000, 213766, 1280428583
              )
            
tab %>% 
gather(type,count,-country,-year) %>% 
arrange(country,year) 
  
tab %>% 
unite('rate',c('cases','population'),sep = "/") 



tab %>% 
select(-population) %>% 
spread(year,cases) 

tab %>% 
select(-cases) %>% 
spread(year,population)  



##########################
## Tidy Data
  ########################        


dat <- presidential
head(dat)


# So many columns. How can we focus on what we want? 
select(dat,name,party)

 
select(dat,name:end)



#The order we select variables translate to the output. 
#Thus, one can easily **reorder columns** with `select()`.

select(dat,name,end,start)


#We can also rename variables by simply providing a new name within the function.

select(dat,president=name,
       startdate=start,
       enddate=end)

# `select()` can  drop variables

select(dat,-start)




### Other useful `select()` behavior

#`contains()` - extract columns with a specific naming convention.
select(dat,contains("a"))



# `starts_with()` - extract columns that start with a specific naming convention.

select(dat,starts_with("s"))


# `ends_with()` - extract columns that end with a specific naming convention.
select(dat,ends_with("d"))

# `everything()` - extract every remaining column not yet stated in the selection. 


select(dat,start,end,everything())


# `matches()` - extract columns using a regular expression.
 select(dat,matches("^s"))


 
 
 ###################
## `filter()`
######################

 # ASks a logical question, them returns the data that matches that condition. 
 filter(dat,party == "Republican")

 # RECALL we also learnt to do this with: 
dat[dat$party =="Republican",]


## `arrange()`
# Orders our data
arrange(dat,party)

# Option desc is descending order. 

arrange(dat,desc(start))



## `mutate()`

# Mutate allows us to:
#a Create new variables that are functions of old varaibles.

mutate(dat,
       # in office during cold war
       CW = start <= '1990-03-11')


#`mutate()` also allows us to **_instantly_** use variables we just created.


mutate(dat,
       CW = start <= '1990-03-11',
       CW = as.numeric(CW))




#Like `mutate()`, `transmute()` provides a method for generating a new variable, but unlike the former, it **returns only the newly created variable**.

transmute(dat,CW = start <= '1990-03-11')



## `summarize()`

summarize(dat,
          days_in_office = mean(end-start),
          max = max(end-start),
          min = min(end-start))

#There are a number of internal functions that can be used with `mutate()`, `transmute()`,
#and `summarize()`.

#- `n()` counts the number of observations
#- `n_distinct()` counts the number of distinct entries


summarize(dat,N=n(),N_party=n_distinct(party))


## `group_by()`

#When used in conjunction with some of the other functions, `group_by()` becomes a powerful to perform by cluster/unit/group operations.

# group by party
x <- group_by(dat,party)
summarize(x,min_in_office = min(end-start))


### Other useful `tidyverse` functions 

count(dat, party)

add_count(dat, party)


mutate(dat,party = 
         recode(party,'Republican'=1,'Democratic'=0)) # Mutate option "recode"

pull(dat,party)



glimpse(dat) # summary stats.


slice(dat,1:2) # Gives us the rows suggested.  
slice(x,2:3) # Works with groups. 


sample_n(dat,2) # A random sample, similar to slice. 

sample_frac(dat,.25)


x <- group_by(dat, party)
x <- arrange(x, start)
slice(x, 1)



# The option `case_when()`
# Operates somewhat like ifelse. 


mutate(dat, 
       pres_type = case_when(
         (end - start) == 1461 ~ "one-term pres.",
         (end - start) == 2922 ~ "two-term pres.",
         TRUE ~ "special case"
       )
)


# Option to lag / lead
mutate(dat,
       predecessor = lag(name,n=1,order_by=start)
)

# useful in groups. 
mutate(x,
       party_predecessor = lag(name,n=1,order_by=start)
)




# Generate unique id. with row_number

mutate(dat,id = row_number())

rownames_to_column(dat,'id')



############################
############## Piping 
##########################

# We can run functions one at a time. 
# This requires us to write, and re-write objects. 
x <- filter(presidential,party=='Republican')
x <- group_by(x,name)
x <- transmute(x,t_in_office = end-start)
x <- arrange(x,t_in_office)
x

#Or we can **nest** functions _within_ each other.

arrange(
  transmute(
    group_by(
      filter(presidential,party=='Republican'),name),
    t_in_office = end-start),
  t_in_office)


## dplyr allows us to do something called pipes. 

#The **pipe** allows us to **pass** output from one function to the next. To pipe:
#- Start with a data object. Then write  **`%>%`** to pass that data to a function. 
#- R will process that data in the function and retain the output. 
#- We can pipe from that output again by re-writing **`%>%`** 


presidential %>%  # Start with data object then apply the pipe %>%
  filter(party=='Republican') %>%   #   Apply the function. Notice, we do not name the dataset. 
## The pipe applies the function and then treats the output as data. So we can apply another pipe to it. 
    group_by(name) %>%
  transmute(t_in_office = end-start) %>%
  arrange(t_in_office)



#########
#####  Joining data
##################



data_A = data.frame("country"=c("Nigeria","England","Botswana"),
                    "Var1"=c(4,3,6),stringsAsFactors = F)
data_B = data.frame("country"=c("Nigeria","United States","Botswana"),
                    "Var2"=c("Low","High","Medium"),stringsAsFactors = F)
data_A
data_B

left_join(data_A,data_B,by="country")



right_join(data_A,data_B,by="country")


inner_join(data_A,data_B,by="country")
full_join(data_A,data_B,by="country")

anti_join(data_A,data_B,by="country")




### Binding rows. 

bind_rows(data_A,data_B)

bind_cols(data_A,data_B)

data_A = data.frame("country"=c("Nigeria","England","Botswana"),
                    'year'=c(1999,2001,2000),
                    "Var1"=c(4,3,6),stringsAsFactors = F)
data_B = data.frame("country_name"=c("Nigeria","United States","Botswana"),
                    'year'=c(1999,2004,2003),
                    "Var2"=c("Low","High","Medium"),stringsAsFactors = F)
data_A
data_B

full_join(data_A,data_B,
          by=c('country'='country_name', # because the country column has different names in ech dataset
               'year'))




#################
### Reshaping Data
#####################



#Often, we need to alter the structure of a `data.frame` from a **wide format**...

D <-
  expand.grid(
  country = c("Nigeria","Iran","Cambodia","Australia"),
  year = c("1992","1993","1994")
)
set.seed(123)
D$var <- round(rnorm(nrow(D),10,.5),2)

# Present as table
D %>% 
  spread(year,var) 

 
D %>% arrange(country) 




### `pivot_longer()`: from wide-to-long
dat <- D %>% spread(year,var) 

dat


---

### `pivot_longer()`: from wide-to-long

dat %>% pivot_longer(cols="1992":"1994")


### `pivot_longer()`: from wide-to-long

#Rename the expanded columns


dat %>% pivot_longer(cols="1992":"1994",
                     names_to = "year",
                     values_to = "ln_gdppc")


### `pivot_longer()`: from wide-to-long

#Be selective about which columns are expanded and which are not. 


# variables can be excluded from the reshape
dat %>% pivot_longer(cols="1992",
                     names_to = "year",
                     values_to = "ln_gdppc") 

## `pivot_wider()`: from long-to-wide


dat <- D %>% rename(ln_gdppc=var)
dat=dat[-2,]
dat=dat[-6,]


dat 



dat %>% pivot_wider(names_from = year,
                    values_from = ln_gdppc)



## `pivot_wider()`: from long-to-wide
#Main arguments:
#- `names_from`: name of the variable that will be spread out into columns. 
#- `values_from`: values that will populate the cells of each column.
#- `values_fill`: specify what value a missing value should take on.

dat %>% pivot_wider(names_from = year,
                    values_from = ln_gdppc,
                    values_fill = list(ln_gdppc = -99))


