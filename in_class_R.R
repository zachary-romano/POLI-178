require(stringr)
require(tidyverse)
require(tidytext)

setwd("~/Downloads/School/POLI 178")
##########################
# Manipulating strings
########################
#  These 

text = "There were 5 cats!
Of these cats, were some cats fluffy? 
Yes, they're fluffy."
text

# A vector of character strings. 
text2 = c("There were 5 cats!", "Of these cats, were some cats fluffy?", "Yes, they're fluffy.")
text2
text3 <- "TeXt MininG iN r"

# This one is organized into a tibble. 
text_data <- tibble(text =   "US opposition politicians and aid agencies have questioned a decision by President Donald Trump to cut off aid to three Central American states --- or so the story reports!")


## This is a dataframe of scraped BBC stories
bbc_data <- read_csv("bbc_output.csv") %>% distinct(url, .keep_all = T)





############
# Inspecting your text
#########################

# Returns T/F: is the pattern in the character string?
str_detect(text,pattern = "cat")
str_detect(text2,pattern = "fluffy")

# Returns the indexes 
str_which(text2,pattern = "fluffy")

# If the pattern is true, it returns the first true text that matches the pattern.   
str_match(text2, "cat")

# It returns every entry that matches the pattern. 
str_match_all(text, "cat")

# Nice visualization 
str_view(text,"cat") # first entry
str_view_all(text,"cat") # All entries. 

 


######################
# Regular expressions (the patterns)
###################


str_view_all(string = text, pattern = "\\d") # digits 
str_view_all(string = text, pattern = "\\w") # letters and numbers
str_view_all(string = text, pattern = "[a-z]") # lowercase letters
str_view_all(string = text, pattern = "[A-z]") # All letters
str_view_all(string = text, pattern = "\\s") # All white spaces (includes spacebar, enter, tab, etc )
str_view_all(string = text, pattern = "\\n") # New paragraph (the enter key)
str_view_all(string = text, pattern = " ") # A space 

str_view_all(string = text, pattern = "[:punct:]") # Common punctuation marks
str_view_all(string = text, pattern = ".") # Anything. 




###### Combinations: ######
str_view_all(string = text, pattern = "[A-Z].") # Capital letter, then next character. 
str_view_all(string = text, pattern = "[A-Z] .") # NOTE: spaces matter...

str_view_all(string = text, pattern = "\\d+\\s+\\w") # a digit, a space and then a word character. 

# The + gives you all the consecutive of the same type. 
str_match_all(string = text, pattern = "[A-z]+")  # Each time you identify a letter, return the string of consecutive letters (until you get a non-letter) 

# The + gives you all the consecutive of the same type. 
str_view_all(string = text, pattern = "[A-z]{6}") # 6 consecutive letters

# ? means the preceding object is optional:  
str_view_all(string = text, pattern = "[A-z]+ ?[A-z]+") # The space is optional
str_view_all(string = text, pattern = "[A-z]+ [A-z]+|[A-z]+") # Equivelent



######## Positioning:
str_view_all(string = text2, pattern = "^\\w") # Does the FIRST character correspond to pattern?   

str_view_all(string = text2, pattern = "[:punct:]$") # Does the LAST character 




 

##################
### Manipulating text:  
######################

### Replacing, extracting.
str_replace_all(string = text, pattern = "cats", replacement = "DOGS")
str_remove_all(string = text, pattern = "[:punct:]") 
str_extract_all(text,pattern = "\\w+")
str_extract_all(text2,pattern = "\\w+") # Applied to the vector of 3 strings. 


# Adding in text:
x <- 5:10
str_c("The value is ",x,"%")
str_glue("The value is {x}%")
str_glue("The value is {x + 5}%")

# Case manipulation
str_to_lower(text3)
str_to_upper(text3)
str_to_title(text3)
str_to_sentence(text3)


####################################  
### Combining / disaggregating: 
#######################

# Let's combine the elements in the vector text2:
str_c(text2, collapse = " \n ") # notice we space out " \n " to put a space. But we could have not done that. 
str_c(text2[1], "They ate my bananas", sep = " \n ")  # Combine 2 elements that are not organized in a vector

## Let's split text into three items. 
str_split(text, '\n')
# NOTICE: It saves as a list. Why?

# Sometimes we are splitting many character strings and we get results of different lengthsL 
str_split(text2, '\\s')


###########################
###################
##  Converting documents into tidytext. 
#################
#######################


### Tokenization 


text_data

text_data %>% 
  unnest_tokens(word,text,token = "characters")

text_data %>% 
  unnest_tokens(word,text,token = "words") # Words are Default


text_data %>% 
  unnest_tokens(word,text,token = "ngrams",n=3)





#### It ieven has twitter functionality. 
#tibble(text = "Hey @professor, this assignment doesn't make sense") %>% 
#  unnest_tokens(word,text,token = "tweets") %>% head(3)

#tibble(text = "Hey @professor, this assignment doesn't make sense") %>% 
 # unnest_tokens(word,text,token = "words") %>% head(3)



#### Stopwords ##### 
#Some words are common, carrying little to no _unique_ information, and need to be removed. 
# `tidytext` comes with a database of common stop words, which we can leverage to remove these low information words.

set.seed(1)
stop_words %>% sample_n(10)

# Drop Stopwords
text_data %>% 
  unnest_tokens(word,text) %>%  
  anti_join(stop_words) %>% #<<
  count(word,sort = T)


##### Stemming  ##### 
#Often words are the same but appear different because of their tense.

txt = "cleaned cleaning cleaner beauty beautiful killing killed killer"
tibble(text = txt) %>% 
  unnest_tokens(word,text) %>% 
  count(word)

#Stemming allows use to reduce a word down to it's fundamental root. ( _Note: Need to install `SnowballC` package_ )

txt = "cleaned cleaning cleaner beauty beautiful killing killed killer"
tibble(text = txt) %>% 
  unnest_tokens(word,text) %>% 
  mutate(word = SnowballC::wordStem(word)) %>% #<<
  count(word)

### Stemming 
text_data %>% 
  unnest_tokens(word,text) %>%  
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word)) %>% #<<
  count(word,sort = T)





########################
# Looking across articles, what are the hot--topics? 
###########################

bbc_data[1,] %>% # This gives us the first row.
  select(headline,url, date)

bbc_data[1,] %>% 
  select(headline,url, date) %>%
  unnest_tokens(word,headline)  # It treats "headline" as text. 
# We now get one row for every word in the headline. 


bbc_data %>%  # NOW WE are using ALL the articles.  
  select(headline,url) %>%
  unnest_tokens(word,headline)  %>% 
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word)) %>%
  count(word,sort = T) 

bbc_data %>%  # NOW WE are using ALL the articles.  
  select(headline,url) %>%
  unnest_tokens(word,headline)  %>% 
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word)) %>%
  count(word,sort = T) 


bbc_data %>%  # NOW WE are using ALL the articles.  
  select(headline,url, date) %>%
  unnest_tokens(word,headline)  %>% 
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word)) %>% #<<
  count(word,sort = T) %>%
  mutate(word = reorder(word, n)) %>%   
  ggplot(aes(word, y=n)) +
  geom_col(show.legend = F) +
  xlab(NULL) +
  coord_flip() +
  theme(text=element_text(size=10))



############################
# Looking at a specific article, what is it about? 
#############################################

bbc_data$story_id <- 1:20

text_data <- bbc_data[1:3,] %>% 
  group_by(story_id) %>% 
  unnest_tokens(word,story) %>% #<<
  ungroup()
text_data


# Term Frequency 

text_data %>% 
  group_by(story_id) %>% 
  count(word,sort=T) 


text_data <-
  text_data %>% 
  anti_join(stop_words) #<<

text_data %>% 
  group_by(story_id) %>% 
  count(word,sort = T) 

#  Let's drop words that have digits in them... using regular expressions and the `stringr` package. 

text_data %>% 
  filter(str_detect(word,"\\d")) %>% 
  select(story_id,word)

#Let's drop words that have digits in them... using regular expressions and the `stringr` package. 

# Further Cleaning 

text_data <- 
  text_data %>% 
  filter(!str_detect(word,"\\d")) 

text_data %>% select(story_id,word)

# Stemming 
text_data <- 
  text_data %>% 
  mutate(word = SnowballC::wordStem(word)) 

# Now count for real
text_data_cnts <- 
  text_data %>% 
  group_by(story_id) %>% 
  count(word,sort=T) %>% 
  ungroup()

text_data_cnts 

text_data_cnts2 <- 
  text_data_cnts %>% 
  bind_tf_idf(word, story_id, n)

text_data_cnts2 %>% 
  select(n,tf,idf,tf_idf)


text_data_cnts2 %>%
  group_by(story_id) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf,fill=story_id)) +
  geom_col(show.legend = F) +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~story_id,ncol=1,scales="free") +
  theme(text=element_text(size=10))


text_data_cnts2 %>%
  group_by(story_id) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf,fill=story_id)) +
  geom_col(show.legend = F) +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~story_id,ncol=1,scales="free") +
  theme(text=element_text(size=10))






################################
#######################
#### Practice: Text manipulation 
##########################
#############################


# 1. Last words. 
# What is the last word of each of the three character strings in text2?


# 2. Sometimes you need to clearn text that looks like this:

newtext <- "The strike, near Kabul airport, killed 10 members of one family, includ-
ing six children, relatives have told the BBC. The US   military said it was    targeting a vehicle carrying at least one person assoc-
iated with the Islamic State's Afghan branch. The US was assessing and investigating   reports of civilian deaths, it added. 

American commanders said there were \"significant secondary explosions\" after the drone strike - implying there were explosives at the scene - which may have harmed people nearby." 


#2.1 Reconnect all of the words separated by '-'. But be sure NOT to remove the "strike - implying". 

#2.2 There are some double spaces and tabs. Make sure to remove them. 

#2.3 Sometimes, words have a \' sign in them. E.g. "State's" . But other times we might see a "didn't" or something like that. Write a piece of code that identifies each unique word
#and treats contraction words as one word. 

#2.4 We are interested in the proper nouns, write a script that pulls out a large number of proper nouns Kabul, US, Afghan, Islamic State, BBC. But not much else 
#(this will never be perfect)


# 3. In our webscraping class, we returned a list of URLs that were not complete. It also included facebook links and other things. It was in this format: 

cite_list <- c("/news/world-46138064","/news/world-asia-59144712", "https://www.bbc.co.uk/news/extra/OAQ9wAGSeh/a-love-letter-to-kabul",
               "/news/world-asia-india-59130556", "https://www.bbc.com/future/article/20211103-the-countries-calling-for-climate-justice",
               "https://www.bbc.com/worklife/article/20211103-the-toll-of-being-left-behind-at-work", "https://www.facebook.com/bbcnews", 
               "https://www.instagram.com/bbcnews")


# Write a function that: 
#       Add https://www.bbc.com/ to the start of the incomplete URls 
#       Otherwise returns the complete BBC URLS
# Then filter out all the non-bbc wefbpages. 



#########################
#### Practice: Tidytext
##########################
# 1.a Break out the BBC articles into a tidytext tibble that treats each word in the article title as a unique row. 
# 1.b Remove the stop words / stem the words / words that include numbers. 
# 1.c Plot the frequency of word stems used in each article.  






#########################################################
##################################################
#  Second lab: Topic models / affinity scores
################################################
####################################################

require(stringr)
require(tidyverse)
require(tidytext)

library(textdata)
library(topicmodels)
data("AssociatedPress") # comes in topic model

setwd("C:/Users/Michael/Dropbox/Sylibus/analytics_intel/data/")

text_data <- read_csv("inaug_speeches.csv")

## NOTE: You'll need to install the sentiment dictionaries
# Run this line and follow the prompts.

################
  ### Sentiment Dictionaries
###############

tibble(
  nrc = get_sentiments('nrc') %>% nrow,
  afinn = get_sentiments('afinn') %>% nrow,
  bing = get_sentiments('bing') %>% nrow,
  loughran = get_sentiments('loughran') %>% nrow,
)

### Sentiment Dictionaries
  
get_sentiments("afinn")

get_sentiments("bing")

  
### Sentiment to text
sent_dict <- get_sentiments("afinn")
sent_text <- text_data %>% inner_join(sent_dict) %>% ungroup()
sent_text



sent_text %>% distinct(word,value) %>% 
  mutate(word = fct_reorder(word,value)) %>% #<<
  ggplot(aes(word, value)) +
  geom_col(show.legend = FALSE,aes(fill=value)) +
  scale_fill_viridis_c() +
  coord_flip() + theme(text=element_text(size=20))

  
text_data %>% ungroup %>% 
  inner_join(get_sentiments("bing"),by = "word") %>% 
  distinct(word,sentiment) %>%
  mutate(word = fct_reorder(word,sentiment=="positive")) %>% 
  ggplot(aes(word, sentiment,label=word,color=sentiment)) +
  geom_text(size=3,show.legend = FALSE) +
  coord_flip() + scale_color_manual(values=c("darkred","steelblue")) +
  theme_minimal() + theme(text=element_text(size=20),axis.text.y = element_blank()) 

inaug_dat <- read_csv("inaug_speeches.csv")
inaug_dat

  ### Example: Inaugural Speeches
  

obama <- 
  inaug_dat %>% 
  filter(Name == "Barack Obama") %>% 
  select(address = `Inaugural Address`,text)

obama

print(obama$text[1])


### Example: Inaugural Speeches

## Let's look at Obama's 2 speaches before he takes office each term. 
# Since tidytext outputs data in order that words are viewed, we can track the tone of speach over time:

obama_txt <- 
  obama %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(address) %>% 
  mutate(index = row_number()) %>% # This indexes words that we kept in order that Obama says them.
  ungroup()
obama_txt


  ### Example: Inaugural Speeches
  
obama_txt %>% 
  ggplot(aes(index,value,fill=address)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~address,ncol = 1, scales = "free_y") +
  theme_minimal() +theme(text = element_text(size=20))



### Let's get some summary stats:
obama_txt %>% 
  group_by(address) %>% 
  summarize(proportion_words_positive = sum(value>0)/n())


###################### What words are frequently said and how do they impact the tone of the speech?
### Example: Inaugural Speeches
obama_txt %>% 
  group_by(word,address) %>% 
  summarize(n = n(),score=max(value)) %>% 
  filter(n>1) %>% 
  ggplot(aes(label=word,size=n,color=score)) +
  ggwordcloud::geom_text_wordcloud_area() + #<<
  scale_color_gradient(low="darkred",high="steelblue") +
  scale_size_area(max_size = 15) +
  facet_wrap(~address,scales="free") +
  theme(text = element_text(size=20))




#############################  
# Topic Models 
###############################

news_data$story[1]


## Topic models
#- What would a human do?
#  + Read the document 
#  + Extract themes

#- Problem: Computers cannot 'know' themes. 
#- Problem: Themes are largely subjective. 


## What a computer does: 

#- Looks at all of the documents in the corpus. 
#- Asks in any one document: 
#  + What words are **commonly found together**?
#  + What words are **rarely found together**? 

#- **A topic** is a list of words that are often together, and not found with other words. 
#  + Each topic weights words as more or less representative of it. 
  
#- Once topics are identified, we score each document for each topic 



# library(topicmodels)
#data("AssociatedPress")
AssociatedPress
AssociatedPress %>% tidy()



### LDA

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
class(ap_lda)

#- Like the other clustering methods that we encountered, `k` is arbitrary. Here we shot for $k = 2$ 

#- Running an LDA is _easy_ (though they are computationally expensive)

# - The challenge lies in **interpreting** the topic output.


# - Extract information regarding topic assignment using the `tidy()` function from the `tidytext` package
#- Parameters of interest
#- "beta" &rarr; Term to Topic
#   - "gamma" &rarr; Document to Topic

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

## Let's get the most commonly used terms. 
ap_top_terms <- 
  ap_topics %>%
  group_by(topic) %>% # Group by the topics
  
  # Grab the top 10 words most 
  # associated with the topic
  top_n(10, beta) %>% 
  
  ungroup() %>% # Ungroup
  arrange(topic, -beta) # Arrange 
ap_top_terms



### Deciphering the Topics

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(text=element_text(size=16))


### Documents to Topics
ap_documents <- tidy(ap_lda, matrix = "gamma") %>% 
  arrange(document,gamma)
ap_documents 

### Documents to Topics

#Document #6 is highly associated with the "politics" topic 
ap_documents %>% filter(document==6)

tidy(AssociatedPress) %>% filter(document == 6) %>% arrange(desc(count))


topic_2 <- ap_documents %>% filter(topic==2 & gamma > .9)
topic_1 <- ap_documents %>% filter(topic==1 & gamma > .9)






