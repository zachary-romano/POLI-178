# Here are some exercises for you to work on the gapminder program with. 
require(tidyverse) # The tidyverse package covered last time 
require(ggthemes) # for great visualization colors and themes. 
require(maps) # for some maps data
require(gapminder)

require(GGally) # great for summary stats. 
require(gghighlight) # emphasize certain variables. 
require(ggrepel) # Nice in-plot labeling. 

####################
# Data and Plan.
########################

#In the wrangling class, we examined the data for gap minder. Here it is again:
gapminder %>% head()

#We used statistical summaries to appreciate variation in the data. Now, let's re-examine the data using visual tools. 

#**Note** This practice will combine the data manipulation tools we developed for wrangling with some visualization cues. 

#**Note** At the end, we will plot a map! Later in the course, we'll come back to map plotting for a more sophisticated look. 

###############################
# Get to know the data through visualization
#########################

#Each of the following questions are targeted at making sure we understand our data better. A great way to get a "feel" for a dataset is to visualize it. 


#### 1. How is `lifeExp`, `pop` and `gdpPercap` variables distributed?
#############

#Remember our two questions from class:
  # Q1. What are we interested in plotting. 
  # A1. A single variable.

  # Q2. What kind of data is it?
  # A2. Numerical data. 

# Let's plot each variable as a histogram. The simplest way to plot these histograms is one at a time. 


# Life Expectancy
gapminder %>% 
  ggplot(aes(lifeExp)) +
  geom_histogram(bins=30) +
  theme_bw() 

# Population
gapminder %>% 
  ggplot(aes(pop)) +
  geom_histogram(bins=30) +
  theme_bw() 

# GDP
gapminder %>% 
  ggplot(aes(gdpPercap)) +
  geom_histogram(bins=30) +
  theme_bw() 

#The better way is to plot all the data at once. We can do this  using the `pivot_longer()` function from last time with `facet_wrap()`

gapminder %>% 
  pivot_longer(cols=c(lifeExp,pop,gdpPercap)) %>%  # we rearrange our data to long form
  ggplot(aes(value)) + # we set to aes to `value` This comes from out pivoted data
  geom_histogram(bins=30) +  # add a layer. 
  facet_wrap(~name,scales="free") +  # facet it by variable type. 
  theme_bw() 

  # Q: Why does this work?

  #To see it, let's view what happens when we pivot longer. 

gapminder %>% 
  pivot_longer(cols=c(lifeExp,pop,gdpPercap))

 #Notice we've stacked lifeExp, pop and gdpPercap in the value column, and then uses the name column to explain which is which. `ggplot` aesthetics can take mappings from a single column `value` and then break out the data by `name`.


# The plots show that there is large right skews in both `gdpPercap` and `pop`. Let's transform these variables and re-plot.


 gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  pivot_longer(cols=c(lifeExp,ln_pop,ln_gdppc)) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins=30) +
  facet_wrap(~name,scales="free") +
  theme_bw() 

# Now let's make things look professional!
  

gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  pivot_longer(cols=c(lifeExp,ln_pop,ln_gdppc)) %>% 
  mutate(name = case_when(
    name == "lifeExp" ~ "Life Expectancy",
    name == "ln_gdppc" ~ "Log GDP Per Capita",
    name == "ln_pop" ~ "Log Population"
  )) %>% 
  ggplot(aes(value,fill=name)) +
  geom_histogram(bins=30,color="white",alpha=.5,show.legend = F) +
  facet_wrap(~name,scales="free_x") +
  labs(caption="Source: gapminder.org") +
  scale_fill_economist() +
  theme_fivethirtyeight() +
  theme(text=element_text(family="serif",face="bold",size=16))
  
##### Life Expectancy

life_box <- gapminder %>%
  ggplot(aes(lifeExp)) +
  geom_density() +
  facet_wrap(~year, scales = "free") +
  theme_bw()

life_box

gapminder %>%
  ggplot(aes(lifeExp, group = continent, fill = continent)) +
  geom_density(alpha = .2) +
  theme(legend.position = "bottom") +
  theme_bw() +
  labs(title = "Life Expectancy by Continent", x = "Life Expectancy", y = "")

gapminder %>%
  ggplot(aes(x= continent, y = lifeExp)) +
  facet_wrap(~year, scales = "fixed") +
  theme_dark() +
  geom_boxplot(outlier.colour = "hotpink") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

########################### 
# `GGally` 
# Another way to summarize
######################

# gggally plots relationships between many variables that help us explore. 

dat <- gapminder %>%
  mutate(ln_gdppc = log(gdpPercap),
         ln_pop = log(pop))

# It can do a simple correlation. 
ggcorr(dat,label = T)

# It can can also auto-generate a huge amount of summary data in a single command:
gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  select(continent,ln_pop,ln_gdppc,lifeExp) %>% 
  GGally::ggpairs(.,progress = F)

# I don't always love this. But it could help sometimes. 




#### 2. What's the relationship between economic development and life expectancy? Is the relationship the same for all continents?

gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  ggplot(aes(ln_gdppc,lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm",se=F) # Let's fit a line to the data.

# Useful but there are a number of small aesthetic adjustments we could make to really help use distinguish between what is going on. 

gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  ggplot(aes(ln_gdppc,lifeExp)) +
  geom_point(alpha=.4,color="grey30") +
  geom_smooth(method = "loess",se=F,color="darkred",size=1.5) +
  theme_minimal()



#Is the trend the same by continent?
  
gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  ggplot(aes(ln_gdppc,lifeExp,color=continent)) +
  geom_point(alpha=.4,) +
  geom_smooth(method = "loess",se=F,size=1.5) +
  theme_minimal()


#Generally speaking, it appears so, but it's difficult to hone in on any one continent. There is just a lot going on. Let's consider two alternative ways of presenting this same data. 

#Way 1: separate plots using `facet_`

gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  ggplot(aes(ln_gdppc,lifeExp,color=continent)) +
  geom_point(alpha=.3,) +
  geom_smooth(method = "loess",se=F,size=1.5) +
  facet_wrap(~continent,nrow=1) +
  labs(x="Log GDP Per Capita",y = "Life Expectancy",color="") +
  scale_color_gdocs() +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size=14,family="serif",face="bold"))

#Way 2: Plot the trends but not the individual data points


gapminder %>% 
  mutate(ln_pop = log(pop),
         ln_gdppc =  log(gdpPercap)) %>% 
  ggplot(aes(ln_gdppc,lifeExp,color=continent)) +
  geom_smooth(method = "loess",se=F,size=1.5) +
  labs(x="Log GDP Per Capita",y = "Life Expectancy",color="") +
  scale_color_gdocs() + # CHANGES colors
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size=14,family="serif",face="bold"))

#### 3. Which countries in Africa have the lowest levels of life expectancy?

# Two ways we could go about answering a question like this. The first might just be an ordered barplot. Here we might rephrase the question as: "Which countries in Africa have the lowest levels of life expectancy _on average_?"


gapminder %>% 
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  summarize(lifeExp = mean(lifeExp),.groups="drop") %>% 
  ggplot(aes(lifeExp,country)) +
  geom_col() 


#Nice, but ordering the factor fields would go a long way. Doing so is easy using the tidy `forcats` package (which is part of the tidyverse), and why we're at it, lets' add a little polish. 


gapminder %>% 
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  summarize(lifeExp = mean(lifeExp),.groups="drop") %>% 
  ggplot(aes(lifeExp,fct_reorder(country,desc(lifeExp)),fill=lifeExp)) +
  geom_col(show.legend = F) +
  scale_fill_gradient2_tableau() + # CHANGES colors
  labs(x="Life Expectancy",y="",
       title = "Average Life Expectancy in Africa",
       subtitle = "1952 - 2007",
       caption = "Source gapminder.org") +
  theme_hc() +
  theme(text=element_text(family = "serif",face="bold",size=14))


#####################
### Getting fancy
#######################
### 4. Mapping in ggplot

# Another way we could approach this is to lay everything out spatially. ggplot with the `maps` package provides a useful way to extract map data on the fly. 

#**note:** We will cover maps in more detail in a later module. 
# This is just a teaser. We'll explain it all later on. 

map_data("world") %>%
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_polygon()

#Our focus is the African continent, so we'll just focus on that portion of the data using the data wrangling principals. 


# Simplify the map data 
world <- 
  map_data("world") %>% 
  select(long,lat,group,country=region) %>% 
  
  # Again standardize the country names
  mutate(country = countrycode::countrycode(country,"country.name","country.name")) %>% 
  mutate(country = ifelse(country == "South Sudan","Sudan",country))

# subset the relevant African countries in the data.
africa <- 
  gapminder %>% 
  filter(continent == "Africa") %>% 
  group_by(country) %>% 
  summarize(lifeExp = mean(lifeExp),.groups="drop")  %>% 
  mutate(country = countrycode::countrycode(country,"country.name","country.name")) %>% 
  inner_join(world,by="country")

#Let's plot the map. 

africa %>% 
  ggplot(aes(x=long,y=lat,group=group)) +
  geom_polygon()


#Now let's fill in the fields on the map using the average life expectancy values.

africa %>% 
  ggplot(aes(x=long,y=lat,group=group,fill=lifeExp)) +
  geom_polygon(color="white",size=.25) +
  scale_fill_gradient2_tableau() +
  theme_map() +
  labs(fill="Life Expectancy",
       title = "Average Life Expectancy in Africa",
       subtitle = "1952 - 2007",
       caption = "Source gapminder.org") +
  theme(text=element_text(family = "serif",face="bold",size=14))


# What if we wanted to look at how these spatial patterns shifted over time? Not a problem, we just need to tweak the data and plot code a little bit. 

# DON'T aggregate the lifeExp variable this time. 
gapminder %>% 
  filter(continent == "Africa") %>% 
  mutate(country = countrycode::countrycode(country,"country.name","country.name")) %>% 
  inner_join(world,by="country") %>% 
  ggplot(aes(x=long,y=lat,group=group,fill=lifeExp)) +
  geom_polygon(color="white",size=.25) +
  scale_fill_gradient2_tableau() +
  theme_map() +
  labs(fill="Life Expectancy",
       title = "Average Life Expectancy in Africa",
       subtitle = "1952 - 2007",
       caption = "Source gapminder.org") +
  facet_wrap(~year) +
  theme(text=element_text(family = "serif",face="bold",size=18),
        legend.position = "bottom")



#########################
#### Highlighting specific observations using`gghighlight`
########################

gapminder %>% 
  mutate(ln_gdppc = log(gdpPercap),
         ln_pop = log(pop)) %>% 
  ggplot(aes(x=year,y=ln_gdppc,color=country)) +
  geom_line(size=.75) +
  gghighlight(max(ln_pop) > 19) +
  theme_minimal()

#####################
# Pretier in-plot labeling using  `ggrepel`
####################

# ggrepel lets us label our points with text.  
gapminder %>% 
  mutate(ln_gdppc = log(gdpPercap)) %>%
  filter(country=="Nigeria") %>% 
  ggplot(aes(ln_gdppc,lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label=year))


dat %>% 
  filter(country=="Nigeria") %>% 
  ggplot(aes(ln_gdppc,lifeExp)) +
  geom_point() +
  geom_label_repel(aes(label=year))

# We can color our labels if we want. Here blue labels represent the post-Cold War. 
dat %>% 
  filter(country=="Nigeria") %>% 
  mutate(cold_war = ifelse(year <= 1990,"yes","no")) %>% 
  ggplot(aes(ln_gdppc,lifeExp,color=cold_war)) +
  geom_point(show.legend = F) +
  scale_color_manual(values=c("steelblue","black")) +
  geom_label_repel(aes(label=year),show.legend = F) +
  ggthemes::theme_economist() +
  labs(x="Log GDP Per Capita",y="Life Expectancy",
       title="Development in Nigeria",
       subtitle = "1952 - 2007",
       caption="Source: Gapminder.org")



