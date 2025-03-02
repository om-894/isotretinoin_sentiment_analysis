
library(tidytext)     # For text mining with tidy data principles
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(tidyr)        # For data tidying
library(ggplot2)      # For data visualization
library(wordcloud)    # For creating word clouds
library(reshape2)     # For reshaping data
library(scales)       # For scaling in plots
library(readr)        # For reading data
library(topicmodels)  # For topic modeling
library(corrplot)     # For correlation plots
library(plotly)       # For interactive plots

# get the Reddit posts and comments dataset

reddit_data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- reddit_data %>%
  group_by(subreddit, post_id, post_title, post_body) %>%  # CHANGE THIS TO post_body ONLY
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# Tokenize the text and remove stopwords ---------------------------------------

# Additional preprocessing
df_tokens <- df_combined %>%
  mutate(full_text = paste(post_title, post_body, comments_combined, sep = " ")) %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(nchar(word) > 2) %>%
  # Add these steps:
  filter(!word %in% c("amp", "http", "https", "com", "www")) %>% # Remove common web artifacts
  filter(!str_detect(word, "^[[:punct:]]+$")) %>% # Remove punctuation-only tokens
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) # Clean remaining punctuation






