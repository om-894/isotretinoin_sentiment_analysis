
# An n-gram is a sequence of n adjacent symbols in particular order.

# This script demonstrates how to analyze relationships between words using n-grams and correlations.
# We will explore tokenizing text into bigrams, filtering and counting n-grams, analyzing bigrams,
# using bigrams for sentiment analysis, visualizing networks of bigrams, and examining word correlations.

# Load necessary libraries -----------------------------------------------------
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(tidyverse)

# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- data %>%
  group_by(subreddit, post_id, post_title, post_body) %>%  # Keep post id, title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# Tokenize the text and remove stopwords ---------------------------------------

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


# Tokenizing by N-gram
# Tokenize the text into bigrams (pairs of consecutive words)
post_bigrams <- df_posts %>%
  group_by(subreddit, post_id) %>%
  unnest_tokens(bigram, post_body, token = "ngrams", n = 2)

# Counting and Filtering N-grams -----------------------------------------------

# Separate the bigrams into two columns
bigrams_separated <- post_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove stop words from both words in the bigrams
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Count the filtered bigrams
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Unite the filtered bigrams back into a single column
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Add counts:
bigrams_united_counts <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

### ADD IN CLEANING HERE (MOVE 'removing less meaningful words' TO HERE)

# Plotting the most common bigrams for each subreddit
bigrams_united_counts %>%
  group_by(subreddit) %>%  # Assuming you have a subreddit column
  arrange(desc(n)) %>%
  slice_max(n, n = 8) %>%  # Get top 8 bigrams per subreddit
  ungroup() %>%
  mutate(bigram = reorder_within(bigram, n, subreddit)) %>%  # Reorder bigrams within each subreddit
  ggplot(aes(bigram, n, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Count") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12)
  )
# MAKE IT GREYSCALE!!

# Analyzing Bigrams ------------------------------------------------------------

# Calculate tf-idf for bigrams
bigram_tf_idf <- bigrams_united %>%
  count(subreddit, bigram) %>%
  bind_tf_idf(bigram, subreddit, n) %>%
  arrange(desc(tf_idf))

# Plotting the highest tf-idf bigrams for each book
bigram_tf_idf %>%
  group_by(subreddit) %>%
  slice_max(tf_idf, n = 7, with_ties = FALSE) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  coord_flip()

# Removing Less Meaningful Words -----------------------------------------------

# We notice that some words like "et", "al", "pubmed.ncbi.nlm.nih.gov" are artifacts 
# or less meaningful.
# Let's remove these words using a custom stop words list.

bigrams_united_clean <- bigrams_united %>%
  filter(
    # Remove URLs and web references
    !str_detect(bigram, "^https?|www\\.|\\.com|\\.gov|\\.edu|\\.org"),
    # Remove file references, numbers, and alphanumeric codes
    !str_detect(bigram, "\\d{4}[a-z0-9]+|file\\s|sheet\\s|\\d+\\s*[a-z0-9]+|[a-z0-9]+\\s*\\d+"),
    # Remove academic reference patterns
    !str_detect(bigram, "article|abstract|doi|pii|^et al|\\sci\\s"),
    # Remove specific patterns you mentioned
    !str_detect(bigram, "isotretinoin https")
  )

# Create a custom stop words list to remove ones that regex could not remove
custom_stop_words <- tibble(bigram = c("drive.google.com", "elta", "ms", "et", "al", 
                                       "pubmed.ncbi.nlm.nih.gov", "ci", "uc", 
                                       "www.accessdata.fda.gov", "youuu"))

# Remove custom stop words from the data
bigrams_united_clean <- bigrams_united_clean %>%
  anti_join(custom_stop_words, by = "bigram")

# Calculate tf-idf for bigrams
bigram_tf_idf <- bigrams_united_clean %>%
  count(subreddit, bigram) %>%
  bind_tf_idf(bigram, subreddit, n) %>%
  arrange(desc(tf_idf))

# Plotting the highest tf-idf bigrams for each book
bigram_tf_idf %>%
  group_by(subreddit) %>%
  slice_head(n = 10) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Bigram", y = "tf-idf") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    axis.text.x = element_text(size = 12),  # Increased x-axis text size
    axis.text.y = element_text(size = 12),  # Increased y-axis text size
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# save the figure
ggsave("figures/reddit_figures/n_gram_and_term_frequency_figures/tf_idf_bigrams_posts.png",
       width = 10, height = 8, dpi = 600, units = "in")

