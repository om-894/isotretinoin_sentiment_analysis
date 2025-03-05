
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

# Topic modeling is a method of unsupervised classification of documents, similar to 
## clustering, which finds natural groups of items. Latent Dirichlet allocation (LDA)
## is a popular method for fitting topic models. Treats each document as a mixture
## of topics and each topic as a mixture of words. Allows documents to overlap each other
## in terms of content, rather than being separated into discrete groups.

# Latent Dirichlet allocation ---------------------------------------------

# Two main principles
# Every document is a mixture of topics: e.g., "Document 1 is 90% topic A and 10%
## topic B, while Document 2 is 30% topic A and 70% topic B

# Every topic is a mixture of words: e.g., two-topic model of politics and entertainment
## may have president, congress, and government as common words in politics
## and movies, television, and actor in entertainment, but budget in both

# Load the abstract data
data <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")

# Filter rows where `has_abstract` is TRUE and remove missing abstracts
abstracts_data <- data %>%
  filter(has_abstract == TRUE, !is.na(abstract)) %>%
  select(pmid, title, abstract, year)

# SPLIT DATAFRAME WHEN CERTAIN MEASURES WERE INTRODUCED???

# IDEA: split abstracts at 2006 because redit founded late 2005 and iPLEDGE was introduced 
# in 2006.This woulkd address the question of whether the introduction of iPLEDGE had an 
# impact on the number of articles published on isotretinoin and a fair timeframe comparison 
# with Reddit.
# Hypothesis: would expect a rise in words such as pregnancy, teratogenic, iPLEDGE, etc.
# Hypothesis: More overall worry and bad stigma about accutane/isotretinoin in socail media 
# posts compared to papers post-2006. Does social media affect papers before and after 2006?

# Split the data into two data frames based on the year
abstracts_data_pre_2006 <- abstracts_data %>%
  filter(year <= 2006)

abstracts_data_post_2006 <- abstracts_data %>%
  filter(year > 2006)

# Make sure to still analyse all 3 dataframes. MOVE THIS TO AFTER TOKENIZATION


# Tokenize the text and remove stopwords from both dataframes ------------------

# Create custom stop words list
custom_stop_words <- c(
  "it's", "i'm", "don't", "that's", "i've", "i'll", "can't", "won't",
  "https", "http", "amp", "com", "www",
  "im", "ive", "id", "ill", "dont", "cant", "wont", "thats",
  "deleted", "removed", "edit", "edited",
  "like", "just", "really", "get", "got", "going", "went",
  "will", "would", "could", "should", "may", "might",
  "one", "two", "three", "first", "second", "third",
  "way", "thing", "things", "something", "anything",
  "much", "many", "lot", "lots",
  "said", "say", "says", "saying",
  "know", "think", "thought", "thinking",
  "even", "still", "also", "else",
  "new", "old", "since", "ago",
  "day", "days", "week", "weeks", "month", "months",
  "want", "wanted", "wanting",
  "make", "made", "making",
  "use", "used", "using", "feel", "its", "pas", "taking"
)

# Combine with built-in stop words
all_stop_words <- bind_rows(
  stop_words,
  data.frame(word = custom_stop_words, lexicon = "custom")
)

# Update your tokenization code
df_tokens <- abstracts_data %>%
  mutate(full_text = paste(title, abstract, sep = " ")) %>%
  unnest_tokens(word, full_text) %>%
  anti_join(all_stop_words, by = "word") %>%  # Use the combined stop words
  filter(!str_detect(word, "^[0-9]+$")) %>%  # Remove numbers
  filter(nchar(word) > 2) %>%  # Remove short words
  filter(!str_detect(word, "^[[:punct:]]+$")) %>%  # Remove punctuation-only tokens
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>%  # Clean remaining punctuation
  filter(!str_detect(word, "^.*\\d+.*$")) %>%  # Remove tokens containing numbers
  filter(!str_detect(word, "^[a-z]{1,2}$"))    # Remove 1-2 letter words

# Create a Document-Term Matrix (DTM) ------------------------------------------

df_dtm <- df_tokens %>%
  count(title, word, sort = TRUE) %>%
  cast_dtm(document = title, term = word, value = n)

# Assess different numbers of topics -------------------------------------------

# Try different numbers of topics for model selection
k_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)
perplexities <- data.frame(k = k_values, perplexity = NA)

for(i in seq_along(k_values)) {
  model <- LDA(df_dtm, k = k_values[i], control = list(seed = 1234))
  perplexities$perplexity[i] <- perplexity(model)
}

# Plot perplexity scores
ggplot(perplexities, aes(x = k, y = perplexity)) +
  geom_line() +
  geom_point() +
  labs(title = "Model Perplexity by Number of Topics",
       x = "Number of Topics (k)",
       y = "Perplexity")

# Based on perplexity scores, the model with 3 topics seems to be the best choice
# While the perplexity continues to decrease after 5 topics, the rate of improvement 
# becomes much more gradual

# We will assess the differences between using 4 and 5 topics.




