
# IF-IDF Explanation
# The TF-IDF score for a term t in a document d within a corpus D is calculated 
# using the following formula:

# TF-IDF(t, d, D) = TF(t, d) × IDF(t, D)

# Term Frequency (TF): Assigns higher weight to terms that appear frequently in a document. 
# If a term appears often in a document, it might be important.

# Inverse Document Frequency (IDF): Assigns higher weight to terms that appear in fewer 
# documents across the corpus. Rare terms across all documents get a higher IDF, 
# highlighting their uniqueness.

# By multiplying TF and IDF, the TF-IDF score balances the frequency of a term in a specific 
# document against its frequency across the entire corpus, effectively highlighting terms 
# that are significant to a particular document.

# HIGH TF-IDF Score: Indicates that the term is very relevant to the document 
# (it appears frequently in that document but not in many documents in the corpus).

# LOW TF-IDF Score: Indicates that the term is common across documents (e.g., common stop words) 
# or does not appear frequently in the document.

# Load necessary libraries
library(tidyverse)    # For data manipulation)
library(dplyr)        # For data manipulation
library(tidytext)     # For text mining with tidy data principles
library(ggplot2)      # For data visualization
library(stringr)      # For string operations


# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- data %>%
  group_by(post_id, post_title, post_body) %>%  # Keep post id, title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# Tokenize the text and remove stopwords ---------------------------------------

# Tokenize the comments into words
comment_words <- df_combined %>%
  unnest_tokens(word, comments_combined) %>%  # Assuming the column is named 'comment_text'
  count(word, sort = TRUE) %>%  # Count word frequencies
  ungroup()

# View word frequencies
head(comment_words, 10)






