# Zachary Romano
# POLI 178
# Homework 2
# 4 October 2021
##############################################################################

rm(list=ls(all=TRUE))

### In class, I discussed some packages to install:

## Make sure we now load them.
require(readr)
require(readxl)
require(haven)


############
#### Remember to set your working directory
###################


# Here is a dataframe of military expenditures and population for 6 countries:
country <- rep(c('Afghanistan', 'Brazil', 'China', 'Australia', 'France', 'Fiji'), each=2)
year <- rep(1999:2000, 6)
mil_exp <-  c(678, 756, 7563, 4389, 1003456, 1306481, 4563,5431, NA, 9342, NA, 134)
population <- c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583, 21435654,21012345, 109034567,100203456, 4542, 44534)

dat <- data.frame(country, year, mil_exp, population)


######################
### Task 1: ifelse
##################
# Here is a list of democracies:
dems <- c("Brazil", "Australia", "France", "Fiji")

# Using the ifelse function create a new variable (democ) equal to 1 if the country is a democracy and 0 otherwise. 
dat$democ <- ifelse(dat$country %in% dems, 1, 0)

############## 
### Task 2: Loops
################

# 1.  We want to normalize the results based on the population average over 2 years. 

# We are going to do it using a loop. (This is not the only way to do it, but we need to practice)
# Write a loop that:
# computes the average population over two years for each country. 
# saves the results.
# Build a dataframe with country  and average population. 

# Create an empty vector called container
container <- vector()

# Save average population in container vector
for (i in 1:nrow(dat)){
  country <- dat$country[i]
  means <- mean(dat$population[dat$country == country])
  container <- c(container, means)
}

# Build a dataframe with country and average population. 
dat2 <- data.frame(unique(dat$country), container[seq(1, length(container), 2)])

# Rename columns
names(dat2) <- c("country", "average_pop")

# View new dataframe
dat2

## 2. Do the same thing for military experience. 
# WITH A TWIST. 
# For each iteration, record whether you found missing data. 
# If you do find missing data, record the result that includes only the data you find. 
# DO NOT use the option na.rm = T. Instead, use an ifelse command. 

# Create an empty vector called container2
container2 <- vector()

# Save average military experience over two years in container2 vector
for (i in 1:nrow(dat)){
  country <- dat$country[i]
  means2 <- ifelse(is.na(dat$mil_exp[i]), NA, mean(dat$mil_exp[dat$country == country]))
  container2 <- c(container2, means2)
}

# Build a dataframe with country and average military experience
dat3 <- data.frame(unique(dat$country), container2[seq(1, length(container2), 2)])

# Rename columns
names(dat3) <- c("country", "average_mil_exp")

# Remove missing values
dat3 <- dat3[!is.na(dat3$average_mil_exp), ]

# View new dataframe
dat3

###################
#######   Task 4: Save and load your package. 
#######################

# 4.1 Save your dataframe to your computer as a .csv file. 
# Remember! You want to save it in the directory for data you developed. 

write.csv(dat, "homework_2_data.csv")

# Run this command to remove all objects/packages from your directory: 

rm(list=ls(all=TRUE))

#4.2 Re-load your packages that you need to read that datafile. 

require(readr)

# 4.3 Read your dataframe back into R. 

dat <- read.csv("homework_2_data.csv")

# Make sure it looks right. 

head(dat)
str(dat)

###########################
# Task 3:  Install the following packages 
# dbplyr
# tidyverse

install.packages("dbplyr")
install.packages("tidyverse")
#############################



