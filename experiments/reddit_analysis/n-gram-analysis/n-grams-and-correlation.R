
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

# Tokenizing by N-gram
# Tokenize the text into bigrams (pairs of consecutive words)
comment_bigrams <- df_combined %>%
  group_by(subreddit, post_body) %>%
  unnest_tokens(bigram, comments_combined, token = "ngrams", n = 2)

# Counting and Filtering N-grams -----------------------------------------------

# Separate the bigrams into two columns
bigrams_separated <- comment_bigrams %>%
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

# Plot most common bigrams


# Analysing Trigrams -----------------------------------------------------------

comment_trigrams <- df_combined %>%
  unnest_tokens(trigram, comments_combined, token = "ngrams", n = 3) %>%
  group_by(subreddit, post_body) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

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
  slice_max(tf_idf, n = 7, with_ties = FALSE) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  coord_flip()
# Figure looks good and regex has worked well. I will manually remove the remaining
# artifacts and less meaningful words myself at a later date.

# Using Bigrams to Provide Context in Sentiment Analysis -----------------------

# Load the AFINN lexicon
AFINN <- get_sentiments("afinn")

# Find words that are preceded by "not" and have a sentiment score
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

# Exploring other negation words
negation_words <- c("not", "no", "never", "without", "quit", "cannot", "cant", "doesn't", "didn't")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  mutate(
    adjusted_value = case_when(
      word1 == "without" & word2 %in% c("pain", "shame", "worrying", "suffering") ~ value,
      word1 == "stopped" & word2 %in% c("hurting", "suffering", "pain") ~ value,
      TRUE ~ -value
    )
  ) %>%
  count(word1, word2, adjusted_value, sort = TRUE) %>%
  ungroup()

# Plotting with adjusted sentiment scores
negated_words %>%
  mutate(contribution = n * adjusted_value,
         word2 = reorder(paste(word1, word2, sep = " "), contribution)) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Negated words") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() +
  facet_wrap(~ word1, scales = "free")

# Figure 4-3. The most common positive or negative words to follow negations such 
# as “never,” “no,” “not,” and “without”


















