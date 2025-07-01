# Zachary Romano
# POLI 178
# Homework 4
# 18 October 2021
#################################################################################

rm(list=ls(all=TRUE))
require(tidyverse)
require(ggplot2) # R's visualization engine. 
require(ggthemes) # for great visualization colors and themes. 
require(ggridges)  # Plots distributions over time. 
require(viridis)  # Nice color palettes. 
require(ggpubr) # Helps put plots together / add features for publication. 


# For your homework, you are going to use the diamonds dataset built into ggplot. 

diamonds <- diamonds

diamonds


## 1. Categorical variables: 

# a. Create two bar chart that visualize the frequency of color and cut respectively. 
ggplot(data = diamonds, aes(x = color)) +
  labs(title = "Frequency of color", x = "Color", y = "Frequency") +
  geom_bar()

ggplot(data = diamonds, aes(x = cut)) +
  labs(title = "Frequency of cut", x = "Cut", y = "Frequency") +
  geom_bar()

# b. For each color, visually represent the proportions of different kinds of cuts in a facet plot. 
cut_freq <- ggplot(data = diamonds, aes(x = cut)) +
  labs(title = "Distribution of cut by color", x = "Cut", y = "Frequency") +
  geom_bar() +
  theme_bw() +
  facet_wrap(~color, scales = "free", ncol = 4)

cut_freq

# c. For the plot in "b" make sure you add a nice label and pick a nice color palette. 
ggplot(data = diamonds, aes(x = cut, fill = color)) +
  geom_bar() +
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Cut", y = "Count", title = "Distribution of cut by color") +
  facet_wrap(~color, scales = "free", ncol = 4)

# 2. Continuous variables. 

# How does the price vary? 

# a. Make one plot of price that summarizes the density of the price outcomes. 
price_density <- ggplot(diamonds, aes(x=price)) +
  labs(title = "Density distribution of price", x = "Price", y  = "Density") +
  theme_bw() +
  geom_density(fill = "skyblue", alpha = .7)

price_density

# b. Make a second plot that summarizes the mean/quantiles.
ggplot(diamonds, aes(x=price)) +
  labs(title = "Price distribution", x = "Price") +
  geom_boxplot(fill = "skyblue", alpha = .7)

# What does the price variance tell us about the distribution of the data? 

# The price variance shows us that the diamond prices within the dataset seem to be skewed to the right. Most of the diamonds have prices within $0 to $5000, and the density drops off significantly past this point.
# The range appears to be between ~200 and 20000, and the density decreases as it goes further to the right edge of the plot. 
# From the boxplot, we can see that th median is slightly below 2500, while Q1 starts near 1000 and Q3 starts just over 5000. This means that 50% of the data falls within this range.

# 3. Relationships

# Transform price to log price. 
diamonds$price <- log(diamonds$price)

# a. We are interested in the price as a function of size. 
# Create one plot that summarizes the relationship between size and price. 
ggplot(diamonds, aes(x = price, y = carat)) +
  geom_point()


# b. We suspect that the relationship varies based on cut. 
# Create one plot that includes several lm lines, where each line represents the relationship between size and price for a specific kind of cut. 
lm_plot <- ggplot(diamonds, aes(x = price, y = carat, color = cut)) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  labs(title = "Relationship between diamond size and price by cut", x = "Price", y = "Carat") +
  geom_point(alpha = .1) +
  geom_smooth(method = lm, se = F)

lm_plot

# Put your (well labeled) plots from 1b, 2a and 3b into one plot and save it. 
plots <- ggarrange(cut_freq, price_density, lm_plot, nrow = 3)

plots

# 4. Upload your r-code txt file to mod3_hw1 
