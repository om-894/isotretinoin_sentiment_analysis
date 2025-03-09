
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
  unnest_tokens(bigram, comments_combined, token = "ngrams", n = 2)






