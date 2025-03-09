
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

# Analysing Trigrams -----------------------------------------------------------

abstract_trigrams <- abstracts_data %>%
  unnest_tokens(trigram, abstract, token = "ngrams", n = 3) %>%
  group_by(period) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analyzing Bigrams ------------------------------------------------------------

# Calculate tf-idf for bigrams
bigram_tf_idf <- bigrams_united %>%
  count(period, bigram) %>%
  bind_tf_idf(bigram, period, n) %>%
  arrange(desc(tf_idf))

# Plotting the highest tf-idf bigrams for each book
bigram_tf_idf %>%
  group_by(period) %>%
  slice_max(tf_idf, n = 7, with_ties = FALSE) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = period)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~period, ncol = 2, scales = "free") +
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
  count(period, bigram) %>%
  bind_tf_idf(bigram, period, n) %>%
  arrange(desc(tf_idf))

# Plotting the highest tf-idf bigrams for each period of abstracts
bigram_tf_idf %>%
  group_by(period) %>%
  slice_max(tf_idf, n = 7, with_ties = FALSE) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = period)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~period, ncol = 2, scales = "free") +
  coord_flip()

# 'Maternal plasma' is crucial in the pharmacokinetics of isotretinoin, as the drug binds 
# extensively to plasma proteins and circulates within the maternal bloodstream. 
# This allows isotretinoin to cross the placenta, exposing the fetus to its highly 
# teratogenic effects. The drug’s persistence in maternal plasma, with a half-life of 
# 10–20 hours and detectable metabolites for weeks, underscores the need for strict 
# pregnancy prevention before, during, and after treatment.

# 'Folic acid' levels are decreased when taking isotretinoin. 
# There may have been more experiments done on this after 2006

# 'mm hg' relates to blood pressure. Potentially less focus on this post 2006.

# Using Bigrams to Provide Context in Sentiment Analysis -----------------------

# Find most common negative words in bigrams and see the word associated after them??
# Then use these as negation_words

# Load the AFINN lexicon
AFINN <- get_sentiments("afinn")

# Find words that are preceded by "not" and have a sentiment score
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

# Exploring other negation words (CHANGE)
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

# Counting and Correlating Pairs of Words with the widyr Package ---------------

# Tokenizing by n-grams allows exploration of adjacent word pairs,
# but sometimes we're interested in co-occurring words within a broader context
# (e.g., within documents or chapters) even if they are not adjacent.

# Counting and Correlating Among Sections --------------------------------------

# 1. Prepare the data
accutane_abstract_words <- abstracts_data %>%
  filter(period == "2006 and later") %>%
  unnest_tokens(word, abstract) %>%
  filter(!word %in% stop_words$word)

# 2. Split into chunks of 1000 words
chunk_size <- 1000
chunks <- split(accutane_abstract_words, 
                ceiling(seq_len(nrow(accutane_abstract_words))/chunk_size))

# 3. Process chunks and combine
word_pairs <- map_df(chunks, function(chunk) {
  chunk %>% pairwise_count(word, period, sort = TRUE)
}) %>%
  group_by(item1, item2) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  arrange(desc(n))

# 4. Examine words appearing with "acne"
word_pairs %>%
  filter(item1 == "isotretinoin")

# Examining Pairwise Correlation -----------------------------------------------

# Not sure if i need to do this or not. 

# To gain a more meaningful insight, we can look at the correlation between words,
# which tells us how often two words appear together relative to how often they appear 
# separately.

# One approach to measure this relationship is using the phi coefficient.
# The phi coefficient is a binary correlation measure that evaluates how likely it is
# that both words (e.g., X and Y) appear together, or both are absent,
# compared to situations where only one of them appears.

# A high phi value indicates a strong positive association (both words tend to 
# appear together), whereas a low or negative phi value suggests a weak or inverse 
# association (one word appears without the other).

# Calculate the phi coefficient between words
word_cors <- accutane_abstract_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, , sort = TRUE)

# Find words most correlated with "results"
word_cors %>%
  filter(item1 == "results")

# Visualize the words most correlated with selected words (Figure 4-8)
word_cors %>%
  filter(item1 %in% c("results", "acne")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Visualize the word correlation network (Figure 4-9)
set.seed(2016)

word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Pairs of words in "Pride and Prejudice"Accutane" subreddit that show at least 
# a 0.5 correlation of appearing within the same subreddit post
