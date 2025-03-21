
# Load necessary libraries------------------------------------------------------

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


### Tokenize abstracts from pubmed----------------------------------------------
# We will tokenize the articles into words and perform sentiment analysis.

# Load the dataset
data <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")

# Filter rows where `has_abstract` is TRUE and remove missing abstracts
abstracts_data <- data %>%
  filter(has_abstract == TRUE, !is.na(abstract)) %>%
  select(pmid, title, abstract, year)

# assign abstracts "pre-2006" and "2006-present" to abstracts based on publication year
abstracts_data <- abstracts_data %>%
  mutate(period = ifelse(year < 2006, "pre-2006", "2006-present"))

# Tokenize the abstracts into words
tokenized_abstracts <- abstracts_data %>%
  unnest_tokens(word, abstract)  # Tokenize the abstracts into words


### NRC lexicon sentiment analysis----------------------------------------------

sentiment_nrc <- tokenized_abstracts %>%
  inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")), by = "word") %>%
  mutate(method = "NRC")

# Filter the NRC lexicon for words associated with "anger"
nrc_anger <- nrc %>%
  filter(sentiment == "anger")

# Filter the NRC lexicon for words associated with "joy"
nrc_joy <- nrc %>%
  filter(sentiment == "joy")

# Find the most common "joy" words in the comments
joy_words <- tokenized_abstracts %>%
  inner_join(nrc_joy, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)              # Count occurrences

# use the most common joy words
top_joy <- head(joy_words, 10)

# plot the most common joy words
top_joy %>%
  top_n(10) %>%  # Take the 10 most common joy words
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "lightblue", color = "lightblue") +  # Blue bars with blue outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "word",
       y = "n") +
  facet_wrap(~ "joy", ncol = 1) +  # Add a title box
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(3, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Find the most common "anger" words in the comments
anger_words <- tokenized_abstracts %>%
  inner_join(nrc_anger, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)                # Count occurrences

# View the most common anger words
top_anger <- head(anger_words, 10)

# plot the most common anger words
top_anger %>%
  top_n(10) %>%  # Take the 10 most common joy words
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "indianred2", color = "indianred2") +  # Blue bars with blue outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "word",
       y = "n") +
  facet_wrap(~ "anger", ncol = 1) +  # Add a title box
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(3, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Combine the joy and anger data
combined_emotions <- bind_rows(
  top_joy %>% mutate(emotion = "Joy"),
  top_anger %>% mutate(emotion = "Anger")
)

# Create combined plot
combined_emotions %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = emotion)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Anger" = "indianred2", "Joy" = "lightblue")) +
  labs(x = "Word",
       y = "Frequency") +
  facet_wrap(~ emotion, ncol = 2, scales = "free_y") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10),
    legend.position = "none"
  )

# Save combined plot
# ggsave("figures/abstract_figures/abstract_top_emotions_combined.png")


### Bing lexicon sentiment analysis---------------------------------------------

# Assign sentiment to words using the Bing lexicon
sentiment_bing <- tokenized_abstracts %>%
  inner_join(bing, by = "word") %>%
  mutate(method = "BING")

# Count positive and negative words for each abstract
abstracts_sentiment <- sentiment_bing %>%
  group_by(pmid, period) %>%                  # Group by each abstract (identified by pmid)
  count(sentiment) %>%                # Count positive and negative words
  spread(sentiment, n, fill = 0) %>%  # Convert to wide format
  mutate(sentiment = positive - negative)  # Calculate net sentiment

# Filter the PMIDs with the most negative sentiments in decending order
most_negative <- abstracts_sentiment %>%
  arrange(sentiment) %>%  # Sort by sentiment
  select(pmid, sentiment)  # Select the PMID and sentiment

# shorten post title so it fits on the graph
most_negative %>%
  head(10) %>%  # Take the 10 most negative abstracts
  ggplot(aes(x = reorder(as.factor(pmid), -sentiment), y = sentiment)) +
  geom_col(fill = "indianred3", color = "indianred3") +  # Red bars with black outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Most Negative Sentiments in posts comments",
       x = "post title",
       y = "Net Sentiment Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Reduce font size

# save the plot
# ggsave("figures/reddit_posts_negative_sentiments.png")

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


### Analyzing Units Beyond Just Words-------------------------------------------
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

### Most Positive and Negative Words contribution for BING----------------------

# Identify words that contribute most to positive and negative sentiment
bing_word_counts <- tokenized_abstracts %>%
  inner_join(bing, by = "word") %>%         # Join with Bing lexicon
  count(word, sentiment, sort = TRUE) %>%  # Count word occurrences by sentiment
  ungroup()

# View the most common positive and negative words
print(head(bing_word_counts, 10))


# Identify words that contribute most to positive and negative sentiment
bing_word_counts <- tokenized_abstracts %>%
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
  labs(x = NULL,
       y = "Frequency") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# The word "patient" may be incorrectly influencing sentiment analysis:
# - "patient" is classified as positive in the Bing lexicon.
# - However, in medical abstracts, "patient" is often neutral and used descriptively, 
#   not as a sentiment indicator.

# To address this issue, we can create a custom stop word list to exclude "patient" 
# from the analysis.

# Custom stop words
custom_swear_words <- tibble(
  word = c("wtf", "patient", "prick"),
  lexicon = "custom"
)

# Combine custom and standard stop words
custom_stop_words <- bind_rows(
  custom_swear_words,
  stop_words
)

# Add "like" as a custom stop word
stop_words <- bind_rows(
  tibble(word = c("like"), lexicon = c("custom")),
  custom_stop_words
)

# View the custom stop words
print(custom_stop_words)

# Tokenize the comments into words, excluding custom stop words
tokenized_abstracts_custom <- abstracts_data %>%
  unnest_tokens(output = word, input = abstract, token = "regex", pattern = "\\s+") %>%
  anti_join(custom_stop_words, by = "word")  # Exclude custom stop words

# Count the most common positive and negative words
bing_word_counts_custom <- tokenized_abstracts_custom %>%
  inner_join(bing, by = "word") %>%         # Join with Bing lexicon
  count(word, sentiment, sort = TRUE) %>%  # Count word occurrences by sentiment
  ungroup()

# View the most common positive and negative words
print(head(bing_word_counts_custom, 10))

# Plot the most common positive and negative words
bing_word_counts_custom %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%                        # Get top 10 words by sentiment
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%     # Reorder words by frequency
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +         # Use columns to represent counts
  facet_wrap(~sentiment, scales = "free_y") +  # Facet by sentiment
  coord_flip() +                          # Flip coordinates for readability
  labs(x = NULL,
       y = "contribution to sentiment") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Save the plot
ggsave("figures/abstract_figures/abstracts_bing_overall_sentiment.png")


### Group abstracts by year to get overall abstract sentiment with BING---------

# Assuming post_sentiment is already calculated
# Select top 10 posts by sentiment for each subreddit
top_abstracts_bing <- abstracts_sentiment %>%
  group_by(period) %>%
  top_n(10, wt = abs(sentiment)) %>%  # Top 10 by absolute sentiment score
  ungroup()

# Plot the data
ggplot(top_abstracts_bing, aes(x = reorder(pmid, sentiment), y = sentiment, fill = period)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ period, scales = "free_y", ncol = 2) +  # Adjust ncol for layout
  labs(x = "PMID", y = "Sentiment") +
  scale_fill_brewer(palette = "Set2") +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )

# Save the plot
ggsave("figures/abstract_figures/top_abstract_sentiments_bing.png")


### Group posts by subreddit to get overall subreddit sentiment with AFINN------

# count afinn sentiment for each comment
abstracts_sentiment_afinn <- tokenized_abstracts_custom %>%
  inner_join(afinn, by = "word") %>%
  group_by(period, pmid) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# get top 10 posts by sentiment for each subreddit
top_abstracts_afinn <- post_sentiment_afinn %>%
  group_by(period) %>%
  top_n(10, wt = abs(sentiment)) %>%  # Top 10 by absolute sentiment score
  ungroup()

ggplot(top_abstracts_afinn, aes(x = reorder(pmid, sentiment), y = sentiment, fill = period)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ period, scales = "free_y", ncol = 2) +  # Adjust ncol for layout
  labs(x = "PMID", y = "Sentiment") +
  scale_fill_brewer(palette = "Set2") +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(3, "pt"),
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )

# Save the plot
ggsave("figures/abstract_figures/top_abstract_sentiment_afinn.png")













