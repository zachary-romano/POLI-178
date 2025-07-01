

###############################################
### Scraping websites
#################################################



install.packages("rvest")
require(rvest) 
url <-  "https://www.bbc.com/news/world-asia-china-57483492"
site <- read_html(url)   #This downloads the webpage into our R environment.
site


## Grabbing the headline using the HTML

headline = site %>% html_node('#main-heading')

headline
# That's not text....
### It's meta data and some other stuff.... 
class(headline)
html_attrs(headline)


## We can use rvest to extract text 
headline %>% html_text(.)



#### That was from the html path "#main-heading"
### ## Grabbing the path from the xml 'xpath'

headline.path =  '//*[(@id = "main-heading")]'  ## This is the xml path.
headline = site %>% html_node(.,xpath = headline.path) %>%
  html_text(.)
headline


### Let's try to grab the body this way. 
body <-  html_nodes(site, '.e1nh2i2l6')
body %>%  html_text(.)

## What's wrong? 


#### Let's take another look. 
# It seems that the paragraphs are layered
body <-  html_nodes(site, '.eq5iqo00')
body %>%  html_text(.)



### Now let's see how well referenced this is. 
## We want to grab all the reference links. 
#One trouble is that the document contains MANY kinds of links. We ONLY want the ones that are refereces.

# How to find the links?
links <-  html_nodes(site, '.e1no5rhv0')
## Ok - let's extract the text.
links %>%  html_text(.)
# Is that the information that we want? 
# Notice, that the text overlays with the main texk. 

links %>% html_attr('href')



### Finally, let's get the date that the article was written. 
date = site %>% 
  html_node(.,xpath = "//time") %>% 
  html_text(.)

date





############################################
#  Your turn: Scraping a list of cites. 
##############################################

# We want to get a list of BBC articles that relate to Asia. 
# That way, we can systematically extract the data from all of them. 

# Here is a cover page on the BBC website. 
url <-  "https://www.bbc.com/news/world/asia"

# Your task: 
# Examine the cite. 
# Compile a list of URLs for BBC news websites.
# Organize that list into a vector. 

site <- read_html(url)
site

cite.list <-  site %>% 
  html_nodes('.nw-o-link-split__anchor') %>% 
  html_attr('href')

cite.list

## It turns out that BBC world news uses a different structure to BBC. 
# So we are going to focus ONLY on BBC World News cites. 
#Also notice that BBC world news cites do not include the www.bbc... part of the address. 

cite.list <- paste("https://www.bbc.com", cite.list[1:41], sep = "")
cite.list

cite.list[-11]


##################What do we have?
### A list of URLs where each website has a similar structure. 
# Our goal is to build a function
# that extracts and organizes info from each webpage


############################################
###  Building a scraper. 
################################################
library(tidyverse)

# We are going to build a function that takes a URL, downloads a webpage, 
# then spits out:
# The header
# The text
#  The date.

bbc_scraper <- function(url){ # Our function takes in an argument 'url'
  # Download website   
  raw = read_html(url)
  # Extract headline
  headline = raw %>% 
    html_node('#main-heading') %>% 
    html_text()
  # Extract date
  date = raw %>% 
    html_node(.,xpath = "//time") %>% 
    html_text(.)
  # Extract Story
  story = raw %>% 
    html_nodes('.eq5iqo00') %>%  html_text(.) %>%
    paste0(.,collapse = " ")  # This paystes all of the paragraphs together in a story.
  # Output as data frame and return
  data.out = tibble(headline,date,story)
  return(data.out)
}



#Let's test our function with a url. 

bbc_scraper("https://www.bbc.com/news/blogs-trending-54121992")


#########################################################
######################### Putting it altogether
#################################################

# Now we have a list of websites we want to scrape.
# A function (scraper) that will extract the relevant parts. 

# We could use an apply function to work through this. 
#But let's use a loop 


output <- c()  # Our container
for(i in 1:length(cite.list)){   # We run the loop the number of times in cite.list.
  temp1 <- bbc_scraper(cite.list[i])   # We apply our function to one url, and save in a temp file 
  output <- bind_rows(output,temp1)  # We bind the results together in our container
}

output

output$url <- cite.list  # Let's add a column so we know where our data came frome. 







##############################################
# Dealing with tables. 
##########################################

library("rvest")

url <- "https://en.wikipedia.org/wiki/List_of_diplomatic_visits_to_the_United_States:_Africa"


africa_visit <- read_html(url)

africa_visit %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table()


### All of the tables.... 
africa_visit %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table') %>%
  html_table()


### Notice that there are no title..... 
### Let's see if we can get them. 
africa_visit %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/h2') %>%
  html_text()




