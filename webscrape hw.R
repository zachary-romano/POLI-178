# The following link is the State Department's website for Papers Relating 
# to Foreign Affairs,
# Accompanying the Annual Message of the President to the Third Session 
# Thirty-seventh Congress
# https://history.state.gov/historicaldocuments/frus1862
# We'll focus on Mexico:
  # https://history.state.gov/historicaldocuments/frus1862/ch12
# Build a function that extracts the (1) title, (2) date, (3) text (4) URL
# of all of the documents that relate to Mexico. Scrape this information and 
# store it in a Tibble.

library(tidyverse)
require(rvest) 

mx_scraper <- function(url){ # Our function takes in an argument 'url'
  # Download website   
  raw = read_html(url)
  # Extract title
  title = raw %>% 
    html_node('#main-heading') %>% 
    html_text()
  # Extract date
  date = raw %>% 
    html_node(.,xpath = "//time") %>% 
    html_text(.)
  # Extract text
  text = raw %>% 
    html_nodes('.eq5iqo00') %>%  html_text(.) %>%
    paste0(.,collapse = " ")
  # Output as data frame and return
  data.out = tibble(title, date, text, url)
  return(data.out)
}

mx_scraper("https://history.state.gov/historicaldocuments/frus1862/ch12")


# Task 2: Twitter API
# Twitter requires you to get a key to use its API.
  # 1. Install the rtweet package.
  # 2. Get a twitter account.
  # 3. Request a key so that you can interface with twitter.
#Here is a page to help you
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-
  #apis/use-twitter-api-r/
library(rtweet)
