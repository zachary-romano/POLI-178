# These exercises help you appreciate the power of random assignment. 

require(tidyverse)
require(gapminder)
set.seed(1) # In R the seed is the starting place for our random number generator. 
# Don't worry, things are still random. It just makes sure you all get the same answer.

# This randomly generates variables following the binomial distribution. 
# The first option is the number of random variables we want generated (e.g. 15) 
# The third option is the probability it is equal to 1. 
rbinom(15, 1, .5)

# To check that it is random, let's do it twice and confirm that the numbers are not correlated.

cov(rbinom(15, 1, .5), rbinom(15, 1, .5))

# That correlation is near 0! Good. 

#################
###### A tangent
####################

#We can generate many kinds of random variables. Here are two common ones
runif(10, min=3, max = 6) # uniform
rnorm(30, mean=3, sd=1) # Normal

#####################

# Let's assume that gapminder represents the entire populatation.
dat  <- gapminder 
# That means, there are no more country years ever in the world (even hypothetically)
# This is obviously not true. But it will help illustrate our point. 

#Let's randomly decide which observations we will "treat" 
dat$treated <- rbinom(nrow(dat), 1, .5)

# I claimed in lecture that PRE-treatment a random sample of the population represented the population.
# Thus, if we randomly assign treatment the treated group and the UNtreaded group will be identical along all covariates.

# Let's remember what the underlying population looks like on 3 variables:
dat %>% 
  pivot_longer(cols=c(lifeExp,pop,gdpPercap)) %>%  
  ggplot(aes(value)) + 
  geom_histogram(bins=50) +  
  facet_wrap(~name,scales="free") +  
  theme_bw() 


# Now let's look at our treated groups and untreated groups:
# Let's remember what the underlying population looks like on 3 variables:
dat %>% 
    pivot_longer(cols=c(lifeExp,pop,gdpPercap)) %>%  
  ggplot(aes(value)) + 
  geom_histogram(bins=50) +  
  facet_wrap(~treated + name  ,scales="free") +  # We are faceting by name AND treatment assignment
  theme_bw() 


# Is the difference between the treated and un-treated groups on our covariates? 


##################
# t-tests and sample sizes. 
################

# I also claimed that t-tests provided the difference between 2 means. 

# lets start with two normally distributed variables
x <- rnorm(20, mean=5, sd=3) # Notice that the means are different. 
y <- rnorm(20, mean=4, sd=3)

frame <- data.frame(treat = c(rep("x", 20), rep("y",20)), value = c(x,y))

ggplot(frame, aes(x=value)) +
  geom_density(aes(color=treat))

# They look a little different, but there is lot's going on. 
# Notice they don't quite look "normally distributed". 
# The reason is that it takes a lot of data for the sample to approximate the population. 

# Let's try a t-test to compute the difference in mean:
t.test(x,y)
# Notice the p-value cannot distinguish the means.


x <- rnorm(10000, mean=5, sd=3) # Notice that the means are different. 
y <- rnorm(10000, mean=4, sd=3)

frame <- data.frame(treat = c(rep("x", 10000), rep("y",10000)), value = c(x,y))

ggplot(frame, aes(x=value)) +
  geom_density(aes(color=treat))
# They look a more different now. Mainly because the distributions have taken on a more stable shape

# Let's try a t-test to compute the difference in mean:
t.test(x,y)
# The t-test confirms that the means are likely different. 
