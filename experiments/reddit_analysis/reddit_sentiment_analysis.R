
library(tidytext)     # For text mining with tidy data principles
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(tidyr)        # For data tidying
library(ggplot2)      # For data visualization
library(wordcloud)    # For creating word clouds
library(reshape2)     # For reshaping data
library(scales)       # For scaling in plots
library(readr)        # For reading data

#### Exploring Sentiment Lexicons ####
# The tidytext package includes several sentiment lexicons that we can use.

# View the sentiments dataset
print(head(sentiments))
# The dataset contains words along with their associated sentiments from different lexicons.

# Get specific sentiment lexicons using get_sentiments()
afinn <- get_sentiments("afinn")  # AFINN lexicon with numeric sentiment scores
bing <- get_sentiments("bing")    # Bing lexicon with positive/negative sentiments
nrc <- get_sentiments("nrc")      # NRC lexicon with various emotions and sentiments

# All three lexicons are based on unigrams, i.e., single words. These lexicons 
# contain many English words and the words are assigned scores for positive/negative 
# sentiment, and also possibly emotions like joy, anger, sadness, and so forth. 
# The NRC lexicon categorizes words in a binary fashion (“yes”/“no”) into categories 
# of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

# View the first few entries of each lexicon
print(head(afinn))
print(head(bing))
print(head(nrc))


#### Perform Sentiment Analysis on the comments and posts from reddit ####
# data was gathered using python script 'subs_and_comments_script.py'
# We will tokenize the articles into words and perform sentiment analysis.

# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- data %>%
  group_by(post_title, post_body) %>%  # Keep post title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# View the result
head(df_combined)

# Looks great. I now need to drop posts with no comments, since they wont be of as 
# much use as pots with comments
df_combined <- df_combined %>%
  filter(comments_combined != "No comments")

# Tokenize the comments into words
tokenized_comments <- df_combined %>%
  unnest_tokens(output = word, input = comments_combined)  # Tokenize the comments into words

# View the tokenized data
print(head(tokenized_comments))


### NRC lexicon sentiment analysis ###
# Filter the NRC lexicon for words associated with "fear"
nrc_fear <- nrc %>%
  filter(sentiment == "fear")

# Find the most common "fear" words in the comments
fear_words <- tokenized_comments %>%
  inner_join(nrc_fear, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)              # Count occurrences

# View the most common fear words
print(head(fear_words, 10))

# Summary of the top 10 most common words associated with the sentiment:
# 
# - `word`: Lists the specific words linked to the sentiment.
# - `n`: Indicates the frequency of each word in the dataset.
#
# Key observations:
# 1. High-frequency words such as "pain" (127) and "bad" (42) 
# 2. Terms like "pain" "anxiety" and "medical" suggest discussions on health risks
# 3. Terms "Depressed" and "anxiety" indicate mental health concerns
#
# Interpretation:
# The most common words associated with fear in the comments are related to health risks,
# mental health concerns, and general anxiety.

