
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

### get the Reddit posts and comments dataset ------------------------------------

# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# In this script moved the code that drops 'NA' values above df_combined to prevent 
# multiple NA values appearing in the same column.
data <- data %>%
  filter(post_body != "NA")  # Drop posts with no post_body

# I want to also keep the title of the post and the subreddit that it belongs to. 
# i need to drop the comments column.

# The posts repeat themselves, so also only have one of each post.
df_posts <- data %>%
  select(subreddit, post_id, post_title, post_body) %>%  # Keep required columns
  distinct(post_body, .keep_all = TRUE)  # Remove duplicate posts


### Tokenize the posts into words and perform sentiment analysis----------------

# Tokenize the posts into words
tokenized_posts <- df_posts %>%
  unnest_tokens(output = word, input = post_body)  # Tokenize the post body into words

# View the tokenized data
print(head(tokenized_posts))






