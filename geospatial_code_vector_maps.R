rm(list=ls(all=TRUE))

#library(readr)
#library(zoo)
library(tidyverse)
#library(dplyr)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
library(kableExtra)
library(countrycode)
library(viridis)
#library(ggrepel)
library(ggthemes)
#install.packages("ggrepel")

setwd("~/Downloads/School/POLI 178")



### Step one. Let's great our map using "sf"
world <- ne_countries(scale = "medium", returnclass = "sf")

## What does world contain
class(world)
str(world)

# Notice it has 
#a. geographical labels, (region, country name / country codes of various kinds.)
# b. some data (population)
# The key variable is geometry 
class(world$geometry)
# It's a polygon!!!! What is that again? 

world$geometry[1]

## Let's plot it. with sf library

ggplot(data=world) + # this gives us the dataset.
  geom_sf()  +  # This plots a geom object.
  theme_classic() 


## We can focus on any part of the world using long / lat: 
ggplot(data=world) + geom_sf() + 
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +   
  theme_bw()

## For our study, we are interested in peacekeeping in Africa in general, and the Mali specifically.  

## THe sf package also has regional covariates associated with each polygon. So we can do this:
AFR_sf <- world %>% filter(region_un == "Africa")


# What did that do? 
AFR_sf$sovereignt

ggplot(data=AFR_sf) + geom_sf() + theme_classic()
## NOTICE: Long and lats are still there. 
## It now focuses us on Africa. 


############################
# Our first dataset.
############################
### One thing we are interested in is where peacekeepers operations are  located. 
## WHat kind of data is that? Is it vector data, or Raster data? 


GeoPKO <- read_csv("pko_geocode.csv")

nrow(GeoPKO)
ncol(GeoPKO)
str(GeoPKO)

### What is the unit of analysis for this data?
# One observation for every (a)  office location (b) for every peacekeeping deployment, (c) at the time that a report was written. 
## If there are many reports written in the same year, we get multiple rows for the same observation...  more on this later.  

## Also, a deployment is a sub-category of a regional HQ, and a country. 

GeoPKO %>% 
arrange(mission,year,location)
str(GeoPKO)
##  Notice all the geographical data:
# Country, long/lat, location (most precise is village), 


## What other data does it have? 
## The number of troops, the type of deployment, the deploying country. 

### Our goal is to focus on:
## Peacekeeping opperations in Africa, in 2018. 
# We also want to know the size of each deployment
# The country it was deployed to
# The mission it is attached to. 


## We are going to get country names from the countrcode package:
AFR_list <- codelist %>% filter(continent %in% "Africa")  %>% select(country.name.en) %>% pull()



### Recall that we have many observations for the same year. We want to make sure that each location is in our dataset once.
# Also, we want an accurate representation of the number of troops over the course of the year. 

GeoPKO2018 <- GeoPKO %>% filter(year==2018) %>%  
  select(mission, year, location, latitude, longitude, no.troops, hq, country) %>%
  mutate_at(vars(latitude, longitude, no.troops), as.numeric) %>% 
  group_by(location, mission, latitude, longitude) %>% 
  mutate(YearlyAverage = round(mean(no.troops, na.rm=TRUE))) %>%  ## Create a new variable that is the average number of troops in a given group (see above line)
  arrange(desc(hq)) %>% ## Arrange in HQ
  slice(1)    ### Take the first in a group


GeoPKO2018_AFR <- GeoPKO2018 %>% 
  mutate(country=case_when(country=="DRC" ~ "Congo-Kinshasa", TRUE~ as.character(country))) %>%  
  filter(country %in% AFR_list)   


## Where are the peacekeeping missions in Africa? 
ggplot(data=AFR_sf) + geom_sf() + 
  geom_point(data = GeoPKO2018_AFR, aes(x=longitude, y=latitude)) + 
  theme_map()  # This is a nice theme, but it takes out the long/lats. 


### Ok - it seems that they are clustered in certain countries and within specific regions of those countries. 
## I think we can learn more. 
## What is the troop-size of each deployment? 

ggplot(data=AFR_sf) + geom_sf() + 
  geom_point(data = GeoPKO2018_AFR, aes(x=longitude, y=latitude, 
                                        size=YearlyAverage, color= YearlyAverage), alpha=.7)


## Remember, each deployment is based out of a HQ,are the deplyoments close to that HQ or not? 

ggplot(data=AFR_sf) + geom_sf() + 
  geom_point(data = GeoPKO2018_AFR, aes(x=longitude, y=latitude, 
                                        size=YearlyAverage, color= YearlyAverage), alpha=.7)+
  scale_size_continuous(name="Average Troop Deployment", range=c(1,12), 
                        breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000)) +
  scale_color_viridis(option="cividis", 
                      breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000), 
                      name="Average Troop Deployment" ) +
  guides( colour = guide_legend()) +
  geom_point(data = GeoPKO2018_AFR %>% filter(hq==3), aes (x=longitude, y=latitude, shape="HQ"),
             fill = "red", size=2, color="red", alpha=.8)+
  scale_shape_manual(values=c(23), labels=c("HQ"="Mission HQ"), name="")+
  geom_label_repel(data = GeoPKO2018_AFR %>% filter(hq==3), aes(x=longitude, y=latitude, label=mission),
                   min.segment.length = 0, 
                   direction="both",
                   label.size = 0.5,
                   box.padding = 2,
                   size = 3, 
                   fill = alpha(c("white"),0.5),
                   shape=16, 
                   size=2) +
  labs(title ="UN Peacekeeping in Africa - 2018", color='Average Troop Deployment') +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.8, l = 4, unit = "cm")),
    panel.grid=element_blank(),
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    legend.key=element_blank()
  )



#########################
# Using facet grids. 
# Peacekeeping operations change over time. How do they change? :
#######################
 
GeoPKO5years <- GeoPKO %>% filter(year%in%c(1994, 2000, 2006, 2012, 2018)) %>%  
  select(mission, year, location, latitude, longitude, no.troops, hq, country) %>% 
mutate_at(vars(latitude, longitude, no.troops), as.numeric) %>% 
  group_by(location, mission, latitude, longitude) %>% 
mutate(YearlyAverage = round(mean(no.troops, na.rm=TRUE))) %>% 
  arrange(desc(hq)) %>% slice(1)

GeoPKO5years <- GeoPKO5years %>% 
  mutate(country=case_when(country=="DRC" ~ "Congo-Kinshasa", TRUE~ as.character(country))) %>% 
  filter(country %in% AFR_list)


ggplot(data=AFR_sf) + geom_sf() + 
  facet_wrap(~year,scales="fixed") +
    geom_point(data = GeoPKO5years, aes(x=longitude, y=latitude, 
                                        size=YearlyAverage, color= YearlyAverage), alpha=.7)+
  scale_size_continuous(name="Average Troop Deployment", range=c(1,12), 
                        breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000)) +
  scale_color_viridis(option="cividis", 
                      breaks=c(0, 100, 300, 500, 1000, 2000, 3000, 4000,5000), 
                      name="Average Troop Deployment" ) + 
  guides( colour = guide_legend()) +
  geom_point(data = GeoPKO5years %>% filter(hq==3), aes (x=longitude, y=latitude, shape="HQ"),
             fill = "red", size=2, color="red", alpha=.8)+
  scale_shape_manual(values=c(23), labels=c("HQ"="Mission HQ"), name="")+
  geom_label_repel(data = GeoPKO5years %>% filter(hq==3), aes(x=longitude, y=latitude, label=mission),
                   min.segment.length = 0, 
                   direction="both",
                   label.size = 0.5,
                   box.padding = 2,
                   size = 3, 
                   fill = alpha(c("white"),0.5),
                   shape=16, 
                   size=2) +
  #labs(title ="UN Peacekeeping in Africa - 2018", color='Average Troop Deployment') +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.8, l = 4, unit = "cm")),
    panel.grid=element_blank(),
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    legend.key=element_blank()
  )






#####################################
## Exercise:  PLot missions in 2018 where:
#1. The size of the mission is the number of troops. 
# The color of the mission is the country it is stationed in. 
##################################



ggplot(data=AFR_sf) + geom_sf() +
  geom_point(data=GeoPKO2018_AFR, 
             aes(x=longitude, y=latitude, size=YearlyAverage, color=country), alpha=.4, shape=20)+
  geom_point(data=GeoPKO2018_AFR %>% filter(hq==3), 
             aes(x=longitude, y=latitude), 
             color="black", shape=16, size=2) +
  geom_label_repel(data=GeoPKO2018_AFR %>% filter(hq==3),
                   min.segment.length = 0.2,
                   label.size = 0.5,
                   box.padding = 2,
                   size = 3,
                   fill = alpha(c("white"),0.7),
                   aes(x=longitude, y=latitude, label=mission)) +
  labs(title="UN Peacekeeping Deployment and Mission HQs in Africa, 2018")+
  scale_size(range = c(2, 16))+
  labs(size="Average number of troops\n(continuous scale)",color="Country",shape="HQ")+
  theme(
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.8, l = 4, unit = "cm")),
    panel.grid=element_blank(),
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    panel.background=element_blank(),
    legend.key = element_rect(fill = "#f5f5f2", color = NA),
    legend.key.size = unit(1, 'lines')
  )+
  guides(colour=guide_legend(ncol=2,override.aes = list(size=5)),
         size=guide_legend(ncol=2))







#############################
# Where should we build peace-keeping posts in Mali?  
######################

### Make a map of Mali national border.

mali_sf <- world %>%  filter(name == "Mali")
ggplot(data=mali_sf) + geom_sf() +
  theme_map()


(mali_sf)


#### We theorize that peace-keeping posts should be located where
# There is a reasonable liklihood of attack. 
######  Past attacks against civillians
##### Civillian protests/riots are common. 
# There is easy access to places of high liklihood of attack. 


### Good news: we already have attack data. it is in ACLED!!!! 

mali_acled <- read_csv("https://raw.githubusercontent.com/edunford/ppol670/gh-pages/Lectures/week_04/walkthrough/acled_africa.csv") %>% 
  filter(country=="Mali") 


mali_acled_2018 <- mali_acled %>% filter(year == 2018)

table(mali_acled_2018$event_type)


#########################################################
#### Your task: 
# plot the ACLED events. 
# Use the "event_type" variable as a color. 
# Only plot Riots, protests and and Violence against civillians. 

mali_civil_acled_2018 <- mali_acled_2018 %>% filter(event_type%in%c("Violence against civilians", "Riots","Protests"))   

ggplot(data=mali_sf) + geom_sf() +  
  geom_point(data = mali_civil_acled_2018, aes(x=longitude, y=latitude, color=event_type), alpha=.5,  position = "jitter")

## What can we learn just from this data! 
# What does government violence seem to do to protests/riots? 


####################################
####### Introducing vector data: roads. 
###################################

roads <- st_read("mali_road/MLI_roads.shp")
glimpse(roads)


## What does roads look like. We will plot the geometry of the road. 
roads %>% ggplot() + geom_sf()
# Notice, that this does NOT include the national bondary. 

# If we want to plot the country border then the roads: 
ggplot(data=mali_sf) + geom_sf() + geom_sf(data=roads,inherit.aes = FALSE) 



## Now let's put it all together. 
ggplot(data=mali_sf) + geom_sf() + geom_sf(data=roads,inherit.aes = FALSE, alpha=.3)  + 
  geom_point(data = mali_civil_acled_2018, aes(x=longitude, y=latitude, color=event_type), alpha=.5,  position = "jitter")


### Now lets see where the peacekeeping stations actually are: 

### Filter our peace-keeping data to MALI 
mali_pkko_2018 <- GeoPKO2018 %>% 
  filter(country == "Mali")


ggplot(data=mali_sf) + geom_sf() + geom_sf(data=roads,inherit.aes = FALSE, alpha=.3,color="black")  + 
  geom_point(data = mali_civil_acled_2018, aes(x=longitude, y=latitude, color=event_type), alpha=.5,  position = "jitter") +
  geom_point(data = mali_pkko_2018, aes(x=longitude, y=latitude, size=YearlyAverage, ), color ="black", shape = 2)


####################################################
# Summary: Introduced the sf mapping system


# Introduced vector data as
# polygons 
# vectors
# points. 

# Discovered several data structures that we can store these data in
# Thought about how to manipulate these structures to make sense of them given our questions. 


### Mapped data as layers and assigned colors, shapes and sizes to help see patterns. 



##################################################################
################ Creating Rasta-like maps from vector data.
#######################################################################

### Let's try and plot ALL the ACLED events in Mali. (not just)

ggplot(data=mali_sf) + geom_sf() +
  geom_point(
    data = mali_acled,
    mapping = aes(
      x = longitude,
      y = latitude
    )
  )


# Yikes! That's a lot of events! 

#### One way to make this pretty is to plot contours. 
############## We can use stat_density_2d to help!

ggplot(data=mali_sf) + geom_sf() +
  stat_density_2d(
    data = mali_acled,
    aes(x = longitude,
        y = latitude))


## lets color it in! 
ggplot(data=mali_sf) + geom_sf() +
  stat_density_2d(
    data = mali_acled,
    aes(x = longitude,
        y = latitude,
        fill = stat(level)),
    geom = "polygon")



ggplot(data=mali_sf) + geom_sf() +
  stat_density_2d(
    data = mali_acled,
    aes(x = longitude,
        y = latitude,
        fill = stat(level)
    ),
    alpha = .2,
    bins = 25,
    geom = "polygon"
  ) +
  scale_fill_gradientn(colors = brewer.pal(4, "YlOrRd"))





#######
## Creating Raster-like maps using adminstrative boundaries. 


admin <- st_read("mali_pop/mli_admbnda_adm3_pop_2017.shp")
glimpse(admin)

## Notice this data is also a little different. 
### It includes a geomtric file (for adminstrative boundaries) 
# It also includes information about the population 


### Questions: 
## How does the population vary across adminsitrative units?
## How does density vary across units?


ggplot(data=admin) + geom_sf(aes(fill=Pop2017_M) )+
  scale_fill_viridis_c(option = "inferno", trans = "log") 


###################
# Other mapping software: GGmap. 
#########################

library(ggmap)
library(RColorBrewer)
library(here)

options(digits = 3)
set.seed(1234)
theme_set(theme_minimal())


chi_bb <- c(
  left = -15,
  bottom = 10,
  right = 5,
  top = 25
)

# retrieve bounding box
mali_gg <- get_stamenmap(
  bbox = chi_bb,
  zoom = 8
)

# plot the raster map
ggmap(mali_gg)





#####################
### Other points. 

## Dealing with Rasta data.
########### Huge files. PRIO is one exception. 

#Cool data
#https://data.humdata.org/dataset

# Cool programs
## ArcGIS. 



###########################################################
## The Raster system
library(countrycode)
library(data.table)
library(rgdal)
library(cshapes)


poly <- cshp(date=as.Date("2014-12-31"), useGW=TRUE) #load map
poly$continent <- countrycode(poly$ISONAME,
                              "country.name", "continent", warn = TRUE) 
gw <- as.vector(na.omit(poly$GWCODE[poly$continent == "Africa"])) #GW country code




prio_yearly$name <- countrycode(as.numeric(prio_yearly$gwno), origin = 'cown', destination = 'country.name')
prio_yearly <- read_csv("mali_road/prio_yearly.csv")

prio_yearly$name <- countrycode(as.numeric(prio_yearly$gwno), origin = 'cown', destination = 'country.name')

mali <- prio_yearly %>%   filter(name == "Mali")


ggplot(data=mali) + geom_sf(aes(fill=Pop2017_M) )+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") 




mali %>% filter(is.na(droughtcrop_speigdm)==F) %>%
  ggplot(aes(fill = droughtcrop_speigdm )) + 
  geom_sf()  
  
  
  
  scale_fill_viridis_c(option = "inferno", 
                       end = .9, 
                       trans = "log10", 
                       labels = scales::comma, 
                       name = "") + 
  labs(title = "GRID-level population in Venezuela") + 
  theme(panel.grid.major = element_blank())




admin <- read_csv("mali_road/PRIO_grid.csv")
glimpse(admin)






#### Finally, let's introduce population at the adminsitrative level. 
admin <- st_read("mali_pop/mli_admbnda_adm3_pop_2017.shp")
glimpse(admin)

## Notice this data is also a little different. 
### It includes a geomtric file (for adminstrative boundaries) 
# It also includes information about the population 


### Questions: 
## How does the population vary across adminsitrative units?
## How does density vary across units?



ggplot(data=admin) + geom_sf(aes(fill=Pop2017_M) )+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") 


ggplot(data=admin) + geom_sf(aes(fill=Pop2017_M) )+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") 




ggplot(data=admin) + geom_sf(aes(fill=Pop2017_M), alpha=.2 )+
scale_fill_viridis_c(option = "plasma", trans = "sqrt")  + 
  geom_sf(data=roads,inherit.aes = FALSE, color="white")  + 
  geom_point(data = mali_pkko_2018, aes(x=longitude, y=latitude, 
                                        size=YearlyAverage, ), color ="blue", alpha=.4)+
  scale_size_continuous(name="Average Troop Deployment", range=c(1,12), 
                        breaks=c(0, 100, 500, 1000))  + 
  geom_point(data = mali_civil_acled_2018, aes(x=longitude, y=latitude), alpha=.3, color="red", shape=2, position = "jitter")




ggplot(data=admin) + geom_sf(aes(fill=Pop2017_M), alpha=.2 )+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")  + 
  geom_point(data = mali_pkko_2018, aes(x=longitude, y=latitude, 
                                        size=YearlyAverage, ), color ="blue", alpha=.4)+
  scale_size_continuous(name="Average Troop Deployment", range=c(1,12), 
                        breaks=c(0, 100, 500, 1000))  + 
  geom_point(data = mali_civil_acled_2018, aes(x=longitude, y=latitude), alpha=.3, color="red", shape=2, position = "jitter")








priogrid <- st_read("priogrid/priogrid_cell.shp")
plot_grid <- ggplot(priogrid) + geom_sf() + theme_map()



