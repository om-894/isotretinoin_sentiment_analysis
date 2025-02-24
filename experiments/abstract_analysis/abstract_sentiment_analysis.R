
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

### Bing lexicon sentiment analysis ###
# Assign sentiment to words using the Bing lexicon
sentiment_bing <- tidy_abstracts %>%
  inner_join(bing, by = "word")

# Count positive and negative words for each abstract
abstracts_sentiment <- sentiment_bing %>%
  group_by(pmid) %>%                  # Group by each abstract (identified by pmid)
  count(sentiment) %>%                # Count positive and negative words
  spread(sentiment, n, fill = 0) %>%  # Convert to wide format
  mutate(sentiment = positive - negative)  # Calculate net sentiment

# View sentiment scores for each abstract
print(head(abstracts_sentiment))

# Filter the PMIDs with the most negative sentiments in decending order
most_negative <- abstracts_sentiment %>%
  arrange(sentiment) %>%  # Sort by sentiment
  select(pmid, sentiment)  # Select the PMID and sentiment

# Filter and plot the top 10 most negative sentiment scores in descending order
most_negative %>%
  head(10) %>%  # Take the 10 most negative abstracts
  ggplot(aes(x = reorder(as.factor(pmid), -sentiment), y = sentiment)) +
  geom_col(fill = "indianred3", color = "indianred3") +  # Red bars with black outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Most Negative Sentiments in Abstracts",
       x = "PMID",
       y = "Net Sentiment Score") +
  theme_minimal()

# save the plot
ggsave("figures/top_10_most_negative_sentiments.png")

# Filter the PMIDs with the most positive sentiments in decending order
most_positive <- abstracts_sentiment %>%
  arrange(desc(sentiment)) %>%  # Sort by sentiment in descending order
  select(pmid, sentiment)  # Select the PMID and sentiment

# Filter and plot the top 10 most positive sentiment scores in ascending order
most_positive %>%
  head(10) %>%  # Take the 10 most positive abstracts
  ggplot(aes(x = reorder(as.factor(pmid), sentiment), y = sentiment)) +
  geom_col(fill = "lightblue", color = "lightblue") +  # Green bars with green outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Most Positive Sentiments in Abstracts",
       x = "PMID",
       y = "Net Sentiment Score") +
  theme_minimal()

#### Analyzing Units Beyond Just Words ####
# Tokenize text into sentences or chapters for sentiment analysis.

# Sentiment using AFINN lexicon
sentiment_afinn <- tidy_abstracts %>%
  inner_join(afinn, by = "word") %>%
  group_by(pmid) %>%                  # Group by abstract
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Combine Bing and NRC sentiments by grouping by `pmid`
sentiment_bing_nrc <- bind_rows(sentiment_bing, sentiment_nrc) %>%
  group_by(method, pmid) %>%               # Group by sentiment method and article
  count(sentiment) %>%                     # Count occurrences of each sentiment
  spread(sentiment, n, fill = 0) %>%       # Convert to wide format (positive/negative)
  mutate(sentiment = positive - negative)  # Calculate net sentiment


# Sentiment using Bing and NRC lexicons
sentiment_bing <- tidy_abstracts %>%
  inner_join(bing, by = "word") %>%
  mutate(method = "Bing")

sentiment_nrc <- tidy_abstracts %>%
  inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")), by = "word") %>%
  mutate(method = "NRC")

# Combine all sentiments (AFINN, Bing, NRC) into one dataset
sentiments_combined <- bind_rows(
  sentiment_afinn,         # Ensure AFINN is grouped by pmid
  sentiment_bing_nrc %>% select(pmid, sentiment, method)  # Select consistent columns
)

# Plot the sentiments for each lexicon (method)
ggplot(sentiments_combined, aes(x = as.factor(pmid), y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +                 # Use columns to represent sentiment scores
  facet_wrap(~method, ncol = 1, scales = "free_y") +  # Facet by sentiment method
  labs(title = "Sentiment Analysis by Lexicon",
       x = "Article (PMID)",
       y = "Net Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),            # Hide x-axis labels for clarity
        axis.ticks.x = element_blank())           # Remove x-axis ticks

# All three lexicons appear to have similar sentiment scores for the articles.
# The sentiment scores are relatively balanced, with a mix of positive and negative sentiments
# relative trajectories across the abstracts, displaying dips and peaks at about 
# the same points. Therefore, despite their differences in absolute measurements, 
# all three lexicons agree on the overall trends in sentiment.

# save the plot
ggsave("figures/sentiment_analysis_by_lexicon.png")

#### Most Common Positive and Negative Words ####
# Identify words that contribute most to positive and negative sentiment

# Identify words that contribute most to positive and negative sentiment
bing_word_counts <- tidy_abstracts %>%
  inner_join(bing, by = "word") %>%         # Join with Bing lexicon
  count(word, sentiment, sort = TRUE) %>%  # Count word occurrences by sentiment
  ungroup()

# View the most common positive and negative words
print(head(bing_word_counts, 10))

# A tibble: 10 × 3
# word         sentiment     n
# <chr>        <chr>     <int>
# 1 severe       negative   1425
# 2 patient      positive   1270
# 3 significant  positive   1235
# 4 risk         negative   1165
# 5 effective    positive   1020
# 6 inflammatory negative    901
# 7 well         positive    846
# 8 adverse      negative    804
# 9 cancer       negative    678
# 10 depression   negative    504

# Plot the most common positive and negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%                        # Get top 10 words by sentiment
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%     # Reorder words by frequency
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +         # Use columns to represent counts
  facet_wrap(~sentiment, scales = "free_y") +  # Facet by sentiment
  coord_flip() +                          # Flip coordinates for readability
  labs(title = "Most Common Positive and Negative Words in Abstracts",
       x = NULL,
       y = "Frequency")

# The word "patient" may be incorrectly influencing sentiment analysis:
# - "patient" is classified as positive in the Bing lexicon.
# - However, in medical abstracts, "patient" is often neutral and used descriptively, 
#   not as a sentiment indicator.

# To address this issue, we can create a custom stop word list to exclude "patient" 
# from the analysis.

# Create a custom stop word list to exclude "patient"
custom_stop_words <- bind_rows(
  data_frame(word = c("patient"), lexicon = c("custom")),  # Add "patient" as a custom stop word
  stop_words                                               # Combine with the standard stop word list
)

#### Creating Word Clouds ####
# Visualize the most common words using word clouds

# Remove stop words
tidy_abstracts_no_stop <- tidy_abstracts %>%
  anti_join(stop_words, by = "word")  # Exclude stop words

# Create a word cloud of the most common words
tidy_abstracts_no_stop %>%
  count(word) %>%                     # Count word occurrences
  with(wordcloud(word, n, max.words = 100))  # Generate a word cloud

# Prepare data for comparison word cloud
word_counts_sentiment <- tidy_abstracts %>%
  inner_join(bing, by = "word") %>%     # Join with Bing lexicon for sentiments
  count(word, sentiment, sort = TRUE) %>%  # Count words by sentiment
  acast(word ~ sentiment, value.var = "n", fill = 0)  # Reshape data for comparison

# Create a comparison word cloud
comparison.cloud(word_counts_sentiment,
                 colors = c("blue", "red"),  # Colors for positive and negative words
                 max.words = 100)           # Limit to 100 most common words

# Create a comparison word cloud for abstracts
tidy_abstracts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%  # Join with Bing lexicon
  count(word, sentiment, sort = TRUE) %>%             # Count word occurrences by sentiment
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%  # Reshape for comparison
  comparison.cloud(colors = c("blue", "red"),         # Colors for positive and negative
                   max.words = 100)                   # Limit to 100 most common words

# save the plot
ggsave("figures/comparison_word_cloud.png")


#### Analyzing Units Beyond Just Words ####

library(dplyr)
library(tidyr)
library(stringr)

# Tokenize abstracts into sentences
abstract_sentences <- abstracts_data %>%
  mutate(sentences = str_split(abstract, "\\.\\s+")) %>%  # Split at full stops followed by space
  unnest(sentences) %>%                                  # Expand sentences into individual rows
  mutate(sentences = str_trim(sentences))                # Remove extra whitespace


abstract_sentences <- abstract_sentences %>%
  filter(sentences != "")

# print a sample sentence
print(head(abstract_sentences))

# We want tot keep the sentences column, so we use drop = FALSE
# unnest_tokens() will create a new column with the tokenized words but will 
# now the original sentences column. Normally it would drop it.

# Tokenize the sentences into words, keeping the sentences column
tidy_sentences <- abstract_sentences %>%
  unnest_tokens(word, sentences, drop = FALSE)

# View the tokenized data
print(head(tidy_sentences))











