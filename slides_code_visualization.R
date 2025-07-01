### This code walks through the example in the lecture slides for Module 3.

require(tidyverse)
# ggplot is part of the tidyverse package. If we load tidyverse, we load ggplot.
require(ggthemes)
# ggthemes will help us with some cool themes later on. 



# We build our plots in class using the diamonds dataset. 
# It s part of ggplot. 
# This is a dataset of diamond sales  

glimpse(diamonds)
# Notice we have some continuous variables, and some 

diamonds

# In the example, we want to examine the relationship between price and carat. 

# Step 1. We 'build' our plot. That is, we tell R that we want to use the dataframe diamonds to construct our data.
ggplot(data=diamonds)
# Look in your plots window. 


# Step 2: We introducing our mapping system into the plot. This tells ggplot what axes are of interest to us. 
ggplot(data=diamonds,aes(x=price,y=carat))
# Look at your plot window. 
# Notice, carat is on the y axis, and price is on the x-axis 
# also notice that the axes are scaled to the values:
summary(diamonds$price)

#####
## A tangent
####
#We can easily change our mapping system by assigning different axes. 
ggplot(data=diamonds,aes(x=carat,y=price))  # Puts price on the y axis. 
ggplot(data=diamonds,aes(x=price)) # Only has an x-axis. 

# We can also instantly manipulate variables in our plot 
ggplot(data=diamonds,aes(x=log(price+1),y=carat)) # Plots the log of price on x axis. 
##########################################################################


# Step 3: Tell ggplot the projection that we want. 
### To do that, we add (literally with a +) to the plot. 
# Since our x/y data is best understood as a unique point, we are going to use a scatter plot. 

ggplot(data=diamonds,aes(x=price,y=carat)) +  # The plus acts like a pipe for ggplot. 
  geom_point()     # THIS LINE tells ggplot the way we want our data plotted. 

#####
## A tangent
####
#  Each projection is an individual layer. We can easily layer these projections. 

# Let's add a line to our plot that summarizes the relationship between price and carat. 
# geom_smooth estimates a line that summarizes the data. 

ggplot(data=diamonds,aes(x=price,y=carat)) +  
  geom_point()+ 
  geom_smooth(method = "lm",se=F, color="red")         # The lm option is the is essentially the bivariate regression line 
                      # The se = F option removes the standard error. 

### We can keep adding things. 
ggplot(data=diamonds,aes(x=price,y=carat)) +  
  geom_point()+ 
  geom_smooth(method = "lm",se=F, color="red") +
  geom_smooth(method = "loess",se=F, color="blue", linetype=2) +  # Loess option fits a local regression. 
geom_vline(xintercept=0, color="orange") +
geom_hline(yintercept=2, color="orange")   # Notice, that it uses the aes mappings set in the first line...


######
# Tangent: We can also add to our plot by manipulating plot features. 

ggplot(data=diamonds,aes(x=price,y=carat)) +  
  geom_point()+ 
  labs(x= "The Price of a Diamond", y = "Carat") + # We can change the labels. 
  theme_hc()  # Themes are built aesthetics that change the scape and background. 


###############################
# Aesthetics
###############################

ggplot(data=diamonds,
       aes(x=price,y=carat, 
           color = cut)) + ## THIS IS THE NEW PART.  
  geom_point(alpha=0.2) 


# Notice that it is inside aes(). 
# We can also set the color for all points if we put color outside aes and localize it to geom_point. 
ggplot(data=diamonds,  aes(x=price,y=carat)) + ## THIS IS THE NEW PART.  
  geom_point(alpha=0.2,color = "red") 


#Here is a more complicated example. Let's color the points white, then add different summary lines conditional on cut. 

ggplot(data=diamonds, aes(x=price,y=carat)) +  
  geom_point(alpha=0.5, color="white") + 
    geom_smooth(method = "loess",se=F, 
              aes(color=cut)) 


# NOTICE the function alpha = .2 makes the dots more transparent. 

