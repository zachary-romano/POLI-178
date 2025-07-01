# Zachary Romano
# POLI 178
# Module 7 Homework
# 2021
#################################################################################

# Clear workspace and set working directory
rm(list=ls(all=TRUE))
setwd("~/Downloads/School/POLI 178")

# Load packages
library(tidyverse)
require(tidytext)
library(dplyr)
library(tm)

## The file bbc_output.csv contains BBC stories on Asia that we scraped from the web. 

#### We are going to practice our text skills using these documents. 

# Load in data
data <- read_csv("bbc_output.csv")


#####################
## 1. Pre-processessing. 
# ##################
#a. For each story, create a story_ID. 
# FOR SIMPLICITY - and to stop your computer for running too long. Let's just focus on the first 8 articles. 

data <- data %>%
  mutate(data, story_ID = row_number())

# b. Some of these stories are pretty long. Let's break them up! Disaggregate the articles into individual sentences. (remember, the questions and ! marks end sentences...)

data$story <- str_split(data$story, "\\?|\\.|\\!")

# c. Assign a sentence_id for each sentence. Each row should  now have 2 IDs!

data <- data %>%
  unnest(story)

data <- data %>%
  group_by(story_ID) %>%
  mutate(sentence_ID = row_number())

# d. ## Process the text by (i) removing the punctuation, stop words, and then stem all the words. (ii) create a bag of words. 

# Remove punctuation
data$story <- str_remove_all(data$story, "[:punct:]") 

# Remove stop words
story %>% 
  unnest_tokens(word, data$story) %>%  
  anti_join(stop_words) %>%
  count(word, sort = T)

story <- data$story[1]

unlist(story)[!(unlist(story) %in% stop_words)]
a <- Corpus(VectorSource(data$story))

story %>%
  unnest_tokens(word, story) %>%
  count(word)

# d. What are the salient words for each sentence? 
    # USE inverse document frequency to find common words for each article. Then visualize!



##################
# 2. Sentiment analysis. 
################

# Let's see if the sentiment of an article changes as the article progresses. 

# 1. Apply the 'bing' sentiment dictionary to each bag of words and create a sentiment score. 

# 2. Create an average sentiment score for the beginning (first 1/3 of the article) middle (2nd third of each article) and end (final 1/3)

# 3. Visualize. 

