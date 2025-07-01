# This code provides some basic examples of plotting. 
# It is designed to walk you through various combinations of data and common visualization choices.  

require(tidyverse)
require(patchwork) # for combining plots
library(palmerpenguins) # https://github.com/allisonhorst/palmerpenguins # A dataset of penguins



##########################
# Basics
####################
# Let's simulate some Data to make things as simple as posssible

set.seed(1234)
dat <- 
  tibble::tibble(var1 = rnorm(1000),
                 var2 = var1 + rnorm(1000))

summary(dat)

dat


### Exploring 1, continuous variable (var1) -----------------------------------------------------



# Histogram
ggplot(data = dat, aes(x = var1)) +
  geom_histogram()

ggplot(data = dat, aes(x = var1)) +
  geom_histogram(binwidth = .01)  # We can make the bin width more precise. 


# Density Plot
ggplot(data = dat, aes(x = var1)) +
  geom_density(fill="blue",color="white",alpha=.5)



#### Exploring patterns for 2, continuous variables ----------------------------------------------------

# Scatter Plot
ggplot(data = dat, aes(x = var1, y = var2)) +
  geom_point()


# Line Plot
ggplot(data = dat, aes(x = var1, y = var2)) +
  geom_line()


# Density Plot (in 2 Dimensions)
ggplot(data = dat, aes(x = var1, y = var2)) +
  geom_density_2d()

# Same thing filled
ggplot(data = dat, aes(x = var1, y = var2)) +
  geom_density_2d_filled()

# Hex Plot
ggplot(data = dat, aes(x = var1, y = var2)) +
  geom_hex()


# Layering Geoms ----------------------------------------------------

ggplot(data = dat, aes(x = var1, y = var2)) +
  geom_point() +
  geom_hex(alpha=.7)


#############################
## Categorical Data
#################

#We'll use the penguins dataset;
## Includes measurements for penguin species, island in Palmer Archipelago,
# size (flipper length, body mass, bill dimensions), and sex. This is a
# subset of penguins_raw.


# Data --------------------------------------------------------------------

dat <- penguins

summary(dat)

# Univariate Categorical --------------------------------------------------

# Bar Plot
ggplot(dat,aes(x=species)) +
  geom_bar()


# Ordering Bar Plot by Frequency
ggplot(dat,aes(x=fct_infreq(species))) +
  geom_bar()


# Adding in more categorical data 
ggplot(dat,aes(x=fct_infreq(species), fill = sex)) +
  geom_bar()


# Stacking vs. Dodge
ggplot(dat,aes(x=fct_infreq(species), fill = sex)) +
  geom_bar(position="dodge")



#### Bivariate: category on continuous -----------------------------------------


# Box plot
ggplot(dat,aes(x=body_mass_g,y = species)) +
  geom_boxplot()


# Violin plot
ggplot(dat,aes(x=body_mass_g,y = species)) +
  geom_violin()


# Jitter plot
ggplot(dat,aes(x=body_mass_g,y = species)) +
  geom_jitter(height = .05,alpha=.5)


# Layer the representations
ggplot(dat,aes(x=body_mass_g,y = species)) +
  geom_jitter(height = .15) +
  geom_violin(alpha=.5)


#### Trivariate: category on category on continous -----------------------------------------

dat %>% 
  drop_na() %>% 
  ggplot(aes(x=sex,y = species,fill=body_mass_g)) +
  geom_tile()


# Trivariate: category on category on continous -----------------------------------------

dat %>% 
  ggplot(aes(x=bill_length_mm,
             y = flipper_length_mm,
             color=species)) +
  geom_point()



################################
## Many plots
########################
# We'll now think how to combine plots together. 


# Data --------------------------------------------------------------------

dat <- penguins

summary(dat)


# Many Plots with Faceting -----------------------------------------


ggplot(dat,
       aes(x=bill_length_mm,
           y = flipper_length_mm,
           color=species)) +
  geom_point() 


# Break plots up by category with facets
ggplot(dat,
       aes(x=bill_length_mm,
           y = flipper_length_mm,
           color=species)) +
  geom_point() +
  facet_wrap(~species) 

# Notice: The facet_wrap makes 1 plot for each species. 


# Adjust scales on the facets
ggplot(dat,
       aes(x=bill_length_mm,
           y = flipper_length_mm,
           color=species)) +
  geom_point() +
  facet_wrap(~species,scales = "free")


# Facet along more than one category
ggplot(drop_na(dat),
       aes(x=bill_length_mm,
           y = flipper_length_mm,
           color=species)) +
  geom_point() +
  facet_wrap(~species + sex)


# Specify the columns and rows
ggplot(drop_na(dat),
       aes(x=bill_length_mm,
           y = flipper_length_mm,
           color=species)) +
  geom_point() +
  facet_wrap(~species + sex,scales = "free",ncol = 2)



# Combine Plots with Patchwork --------------------------------------------

plt1 <- 
  ggplot(dat,aes(x=bill_length_mm,
                 y=flipper_length_mm)) +
  geom_point()
plt1

plt2 <- 
  ggplot(dat,aes(x=fct_infreq(species), fill = sex)) +
  geom_bar(position="dodge")
plt2


# Combine them with patchwork
plt1 + plt2

# Arrange them
plt1 + plt2 + plot_layout(ncol = 1)


# resize them
plt1 + plt2 + plot_layout(ncol = 1,heights = c(.25,.75))


# sky is the limit
plt1 + plt2 + plt2 + plt1 


###########################  
## Customizing Plots
#############################

# Includes measurements for penguin species, island in Palmer Archipelago,
# size (flipper length, body mass, bill dimensions), and sex. This is a
# subset of penguins_raw.


# Data --------------------------------------------------------------------

dat <- penguins

summary(dat)


# Customizing Plot Aesthetics -----------------------------------------

ggplot(dat,
       aes(x=bill_length_mm,
           y = flipper_length_mm,
           color=species)) +
  geom_point(size=3,alpha=.75) +
  scale_color_manual(values = c("darkred","steelblue","grey30")) +
  theme_classic() +
  labs(x = "Bill Length (mm)",y="Flipper Length (mm)",color="") +
  theme(legend.position = "top")



######################################
# Practice problems
########################
require(tidyverse)

dat <- diamonds 
head(dat)


#Using the `diamonds` data, and a bar plot, plot the count of the different `cut` categories. Customize the plot as follows:
    #- Change the `fill` of the bars be `"steelblue"`;
    #- Change the theme of plot to be `minimal`;
    #- Add a title using `labs()` that reads "Diamond Cuts"



## Question 2  {.tabset}

#Do the following:
  #- Create a new variable in the `diamonds` data called `expensive` that takes on the value of `"yes"` if the price is above the average price of a diamond, `"no"` otherwise. 
  #- Using the same code you used to generate the plot in Q1, use this variable to break up each category into expensive and non-expensive stacks using the `"dodge"` position (see the video on categorical data). 
  #- When `expensive == "yes"` the bars should be filled `orange`, and `grey30` otherwise. 


      ### Hint, you can change the color using the command  scale_fill_manual(values = c()) 




## Question 3  {.tabset}

#Using the `diamonds` data, plot `price` on `carat` using points, and do the following:

#- Make the points transparent using `alpha = .25`;
#- Color each point by `cut`. 
#- Facet the plots by `cut` and arrange the facet plots so that you only have 1 row;
#- Change the labels so that the legend reads "Diamond Cut", the x-axis reads "Price ($)" and the y-axis reads "Carat". 
#- Change the theme to `classic`;
#- Change the position of the legend so that it's on the `"bottom"` of the plot. 


