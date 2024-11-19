
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

#### Perform Sentiment Analysis on the articles from pubmed ####
# We will tokenize the articles into words and perform sentiment analysis.

# Load the dataset
data <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")

# Filter rows where `has_abstract` is TRUE and remove missing abstracts
abstracts_data <- data %>%
  filter(has_abstract == TRUE, !is.na(abstract)) %>%
  select(pmid, title, abstract)

# Tokenize the abstracts into words
tidy_abstracts <- abstracts_data %>%
  unnest_tokens(word, abstract)  # Tokenize the abstracts into words

# View the tokenized data
print(head(tidy_abstracts))

# View the updated data
print(head(tidy_abstracts))

### NRC lexicon sentiment analysis ###
# Filter the NRC lexicon for words associated with "joy"
nrc_fear <- nrc %>%
  filter(sentiment == "fear")

# Find the most common "joy" words in the abstracts
fear_words <- tidy_abstracts %>%
  inner_join(nrc_fear, by = "word") %>%  # Join with joy words
  count(word, sort = TRUE)              # Count occurrences

# View the most common joy words
print(head(fear_words, 10))

# Summary of the top 10 most common words associated with the sentiment:
# 
# - `word`: Lists the specific words linked to the sentiment.
# - `n`: Indicates the frequency of each word in the dataset.
#
# Key observations:
# 1. High-frequency words such as "disease" (1,482) and "risk" (1,165) highlight 
# the focus on medical research.
# 2. Terms like "adverse," "cancer," and "tumor" suggest discussions on health risks 
# and conditions.
# 3. Words related to treatments ("treat") and clinical contexts ("diagnosis") occur 
# less frequently but are significant.
#
# Interpretation:
# - The dataset primarily addresses topics such as disease risk, adverse outcomes, 
# and medical conditions, reflecting a strong focus on healthcare and research.


