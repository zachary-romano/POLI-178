rm(list=ls(all=TRUE))

setwd("...analytics_intel/Modules/2_R_bootcamp/in_class_examples") 
# This is my wd for teaching bootcamp lectures. 
#My structure is a little different because I divide teaching materials and analytical materials. 

getwd() # IF you want to learn where R sets your wd
## If you want to learn more about r project management, see the set_up slides on CANVAS. 


# Usually, we'd have our packages here. But we don't have any....

# Usually, we'd import our raw data here. But we don't have any. 



############
## Some points on objects/classes:
###########
# The lectures covered different classes. I want to emphasize some things about
# Coercion
# factors/characters. 

vec_1 <- c(1,4,6,7) # A vector of numbers
class(vec_1)  # It's class 
str(vec_1)  # structure. 

vec_1[3] <- "medium"  # let's replace the number 6 with the word "six"
str(vec_1)
class(vec_1) # Notice the structure changes for ALL elements. 
# We call this "coercion"

countries <- c("ITA", "JAM", "JAM", "JAM", "USA", "USA", "CAD", "UK", "USA")
str(countries) # A vector or countrie names stored as characters. 
table(countries)          

countries_fac <- as.factor(countries)
# We now coerce it to factors. 
countries_fac

# When we print it, notice the "levels". 
# What R is doing is (a) identify unique entries. (b) assign each unique entry a number. (3) re-store the vector as numbers. 


# Factors print like characters, but they opperate much more like numbers. 

# Here is how they appear like characters. 
table(countries_fac)

# But sometimes we get problems.... 

# But here is how they behave like numbers. 
as.numeric(countries_fac)

# This can get us into trouble:
sum(as.numeric(countries_fac))  # This sums the level assignments. This makes no sense. 
# Sometimes we do stuff like this by accident and it leads to weird data outcomes. 

# Another problem with facotrs is that we assume an order. 
# For example, we intuitively assume that January  = 1. But R doesn't do that. 
# Consider this vector: 

 mons = c("March","April","January","November","January",
 "September","October","September","November","August",
  "January","November","November","February","May","August",
  "July","December","August","August","September","November",
    "February","April")

mons <-  factor(mons) # Convert into a factor. 

table(mons)  # Notice April is first. 

levels(mons)  # R assigns the factors alphabetically. 

# This function does two things. 
# 1. It "re-order" our factors using the  option levels=c() 
# 2. It tells R that the order is meaningful with option ordered=TRUE
mons = factor(mons,
          levels=c("January","February","March", "April","May","June","July","August","September",
          "October","November","December"),  
          ordered=TRUE)  # Tell R that the factors are ordered. 

table(mons)
mons[1] < mons[5]

#####################
#### Data-frames
#######################

#Let's make a DF of countries 
# This DF could be useful if we are interested in the effects of GDP on protests. 

Protests <- c(1, 2, 4, 5, 44, 6, 10)
GDP <- c(31, 74, 3, 7, 100, 234, 41)
country_name <- c("Uganda", "Australia", "UK", "USA", "Fiji", "Nevis", "India")
dat_protest <- data.frame(country_name,Protests,GDP)

str(dat_protest)

# Notice the object has a class (dataframe), AND each column has a different class. 

# DFs are useful for data analysis because they have a rigid structure:
#a. each row represents an observation (a country in this case)
# b. Each column represents a variable (e.g. how many protests in that country, or the country's GDP)
# We make this clear through assigning rows IDs - unique identifiers for each row - and column a "key" or column name. 

# We can access the column names (or variables) using
names(dat_protest)


###############
####### Loops
################


# See slides for description of this lool  
fruits <- c("bananas","apples","grapes","pears","oranges") # A list of fruits. 

#I want to say "I like bananas", "I like apples"... etc
# Loops let me do this by cycling through the list in a loop. 

for( i in fruits ){ # i takes on the value of fruit one at a time. 
  cat("I like",i,"\n")  # "\n# prints a new line. 
}

## the command i in fruits runs the loop once for every element in fruit. 

## We can have more control over our loops using:
#a. a list of integers and  
# b. an indexing system fruits[i]

#For example:
1:5 # intigers 1 to 5. 
1:length(fruits) # intigers 1 to the length of fruits.   


## This loop generates an identical result as the one above: 
for( i in 1:length(fruits) ) { # i takes on the value of fruit one at a time. 
  cat("I like",fruits[i],"\n")  # "\n# prints a new line. 
}


#################
### Recursive object are VERY IMPORTANT for loops.
####################
# R lets you write and re-write an object. 

x <- 100 # I can write the object
x <- 50 # I can then replace it with something else!

#A very useful feature is that we can write objects as functions of themselves:

x <- x/2 # This basically says, whatever number "x" is, divide it by 2, then replace x with that number.

# Say we wanted to know what happens if we cut 100 in half 100 times: 

x <- 100  # We set our initial data point


for( i in 1:100 ) { # Repeats our loop from 1:100 (i.e. 100 times)
  x <- x/2   # Each loop cuts x in half and replaces x with the answer.  
}

x


# We can use the same logic to add to objects: 
countries <- c(countries, "new_country")  
# In this example we took the object "countries" a list of countries. 
#We then use the function that binds vectors "c()" to say the following:
# Replace the object countries with an object that binds our original countries and a new country. 
# You can run this over and over again, and the object keeps growing by one "new_country"

# Say we want to record all of the answers for cutting x in half: 

container <- vector() #We build a container - an empty vector. 

x <- 100  # Set our initial datapoint

for( i in 1:100 ) { # Repeats our loop from 1:100 (i.e. 100 times)
  x <- x/2   # Each loop cuts x in half and replaces x with the answer.  
container <- c(container, x)
  }

container


#####################
#### Logical operators. 
###################
# R lets us ask yes no questions. 
# Recall our object Protests is the number of protests in a country 

Protests

# This command asks, for each country, is the number of protests greater or equal to 10?
Protests >= 10

#This command asks less than 5?
Protests < 5

  # The answer is a booleen object (takes on True false values) We can instantly convert these into numbers:
  (Protests >= 10 )*1 

## If we want to do the same thing but have more specific answers we can use function ifelse().

#ifelse(our question , What to do if our question is true, what to do if our question is false)

ifelse(Protests >=10 , "big", "small")

# Not only prints. It can also manipulate / spit out numbers. 
ifelse(Protests >=10 , Protests*3, "small")


# If we have lots of conditions, we can combine logical statements with loops. 
# In this example we are going through each row in our protest DF. 
# For each row we ask is the protest bigger than 10, or smaller than 5 OR otherwise. 
# We have three different responses for each condition: 

container <- vector()
for( i in 1:nrow(dat_protest)){
    if(dat_protest$Protests[i] > 10) {
      container <- c(container, "Big")  }
        else if (dat_protest$Protests[i] < 5) {
        container <- c(container, "Small")
                    } else {
  container <- c(container, "Medium")
}}

dat_protest$protest_size <- container

dat_protest



## NAs. 

#Remember me:
vec_1


vec_1_f <- as.factor(vec_1)
vec_1_n <- as.numeric(vec_1)


sum(vec_1_n)

sum(vec_1, na.rm = T)

table(vec_1)

as.factor(vec_1)

dat_protest$Protests[dat_protest$Protests < 0]


#################
#####3Packages to install
##############################
## See announcement / lecture slides on what packages are. 
install.packages("tidyverse")
install.packages("readr")
install.packages("readxl")
install.packages("haven")
install.packages("lubridate")


#####################
######## Saving your work. 
####################

# We have created lots of objects. R lets us save them all as an R file. 
save.image("all_class.Rdata")
# This is great if you want to pick up in R exactly where you left off. 
# But if you want to use the data in a different project / program, this is not good. 

# If you just want to save one dataframe, csv is a good format. 
# This command requires the readr package. 
write_csv(dat_protest, "dat_protest.csv")

#Question: Where did it save? 
# Answer: 
getwd()
# If you want it to save somewhere else you have two options. 
#1. Set a new wd. 
# 2. Overwrite your wd in the command:
write_csv(dat_protest, "C:/newplacetosave/subfolder/subsubfolder/dat_protest.csv")

# If you want to save to a subdirectory of your wd simply include it in the name:
write_csv(dat_protest, "subfolder/dat_protest.csv")


