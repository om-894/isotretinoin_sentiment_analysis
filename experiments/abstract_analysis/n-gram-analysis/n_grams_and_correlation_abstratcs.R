
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

# Load data --------------------------------------------------------------------

data <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")

# Filter rows where `has_abstract` is TRUE and remove missing abstracts
abstracts_data <- data %>%
  filter(has_abstract == TRUE, !is.na(abstract)) %>%
  select(pmid, title, abstract, year)

# SPLIT DATAFRAME WHEN CERTAIN MEASURES WERE INTRODUCED???
# Dont have to split, just add a column for groups

# What is the eariest year in the dataset?
min(abstracts_data$year) # 1987

# The latest year is 2024. We will split the data into 2006=< and 2006>
# Fortunately, halfway in the time periods is 2006, which also was the introduction
# of the iPLEDGE program and not long after reddit was founded.

abstracts_data <- abstracts_data %>%
  mutate(period = ifelse(year >= 2006, "2006 and later", "Before 2006"))


# Tokenizing by N-gram ---------------------------------------------------------

# Tokenize the text into bigrams (pairs of consecutive words)
abstract_bigrams <- abstracts_data %>%
  group_by(period, abstract) %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2)

# Counting and Filtering N-grams -----------------------------------------------

# Separate the bigrams into two columns
bigrams_separated <- abstract_bigrams %>%
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
  group_by(period) %>%  # Assuming you have a subreddit column
  arrange(desc(n)) %>%
  slice_max(n, n = 8) %>%  # Get top 8 bigrams per subreddit
  ungroup() %>%
  mutate(bigram = reorder_within(bigram, n, period)) %>%  # Reorder bigrams within each subreddit
  ggplot(aes(bigram, n, fill = period)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Count") +
  facet_wrap(~period, ncol = 2, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 12)
  )

# 'severe acne' is quite prevalent in 2006 and later. The word 'isotretinoin' also
# Appears in both time periods, but more so in 2006 and later.
# '13 cis' relates to retinoic acid. 'mg kg' relates to dosage.






