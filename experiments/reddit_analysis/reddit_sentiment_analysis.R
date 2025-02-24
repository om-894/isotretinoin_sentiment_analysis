
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


#### Perform Sentiment Analysis on the comments and posts from reddit ####
# data was gathered using python script 'subs_and_comments_script.py'
# We will tokenize the articles into words and perform sentiment analysis.

# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- data %>%
  group_by(post_id, post_title, post_body) %>%  # Keep post id, title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# View the result
head(df_combined)

# Looks great. I now need to drop posts with no comments, since they wont be of as 
# much use as pots with comments
df_combined <- df_combined %>%
  filter(comments_combined != "No comments")


#############################################################################
#       Tokenize the comments into words and perform sentiment analysis     #
#############################################################################

# Tokenize the comments into words
tokenized_comments <- df_combined %>%
  unnest_tokens(output = word, input = comments_combined)  # Tokenize the comments into words

# View the tokenized data
print(head(tokenized_comments))


### NRC lexicon sentiment analysis ###
sentiment_nrc <- tokenized_comments %>%
  inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")), by = "word") %>%
  mutate(method = "NRC")

# Filter the NRC lexicon for words associated with "fear"
nrc_fear <- nrc %>%
  filter(sentiment == "fear")

# Find the most common "fear" words in the comments
fear_words <- tokenized_comments %>%
  inner_join(nrc_fear, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)              # Count occurrences

# View the most common fear words
print(head(fear_words, 10))

# Summary of the top 10 most common words associated with the sentiment:
# 
# - `word`: Lists the specific words linked to the sentiment.
# - `n`: Indicates the frequency of each word in the dataset.
#
# Key observations:
# 1. High-frequency words such as "pain" (127) and "bad" (42) 
# 2. Terms like "pain" "anxiety" and "medical" suggest discussions on health risks
# 3. Terms "Depressed" and "anxiety" indicate mental health concerns
#
# Interpretation:
# The most common words associated with fear in the comments are related to health risks,
# mental health concerns, and general anxiety.

### Bing lexicon sentiment analysis ###
# Assign sentiment to words using the Bing lexicon
sentiment_bing <- tokenized_comments %>%
  inner_join(bing, by = "word") %>%
  mutate(method = "BING")

# Count positive and negative words for each abstract
post_sentiment <- sentiment_bing %>%
  group_by(post_id, post_title, post_body) %>%                  # Group by post (identified by post_title)
  count(sentiment) %>%                # Count positive and negative words
  spread(sentiment, n, fill = 0) %>%  # Convert to wide format
  mutate(sentiment = positive - negative)  # Calculate net sentiment

# View sentiment scores for each posts' comments
print(head(post_sentiment))

# Looks interesting. The negative posts are definately showing the lowest sentiment scores.

# Filter the posts with the most negative sentiments in decending order
most_negative <- post_sentiment %>%
  arrange(sentiment) %>%  # Sort by sentiment
  select(post_id, post_title, sentiment)  # Select the post_id, post_title and sentiment

# shorten post title so it fits on the graph
most_negative %>%
  head(10) %>%  # Take the 10 most negative abstracts
  ggplot(aes(x = reorder(as.factor(post_id), -sentiment), y = sentiment)) +
  geom_col(fill = "indianred3", color = "indianred3") +  # Red bars with black outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Most Negative Sentiments in posts comments",
       x = "post title",
       y = "Net Sentiment Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Reduce font size

# save the plot
# ggsave("figures/reddit_comments_negative_sentiments.png")

# Filter the PMIDs with the most positive sentiments in decending order
most_positive <- post_sentiment %>%
  arrange(desc(sentiment)) %>%  # Sort by sentiment in descending order
  select(post_id, post_title, sentiment)  # Select the PMID and sentiment

# View sentiment scores for each posts' comments
print(head(most_positive))

# Filter and plot the top 10 most positive sentiment scores in ascending order
most_positive %>%
  head(10) %>%  # Take the 10 most positive abstracts
  ggplot(aes(x = reorder(as.factor(post_id), sentiment), y = sentiment)) +
  geom_col(fill = "lightblue", color = "lightblue") +  # Green bars with green outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Most Positive Sentiments in post comments",
       x = "post title",
       y = "Net Sentiment Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

#### Analyzing Units Beyond Just Words ####
# Tokenize text into sentences or chapters for sentiment analysis.

# Sentiment using AFINN lexicon
sentiment_afinn <- tokenized_comments %>%
  inner_join(afinn, by = "word") %>%
  group_by(post_id, post_title) %>%                  # Group by abstract
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Combine Bing and NRC sentiments by grouping by `post_id`
sentiment_bing_nrc <- bind_rows(sentiment_bing, sentiment_nrc) %>%
  group_by(method, post_id, post_title) %>%               # Group by sentiment method and article
  count(sentiment) %>%                     # Count occurrences of each sentiment
  spread(sentiment, n, fill = 0) %>%       # Convert to wide format (positive/negative)
  mutate(sentiment = positive - negative)  # Calculate net sentiment


# Combine all sentiments (AFINN, Bing, NRC) into one dataset
sentiments_combined <- bind_rows(
  sentiment_afinn,         # Ensure AFINN is grouped by pmid
  sentiment_bing_nrc %>% select(post_id, post_title, sentiment, method)  # Select consistent columns
)

# Plot the sentiments for each lexicon (method)
ggplot(sentiments_combined, aes(x = as.factor(post_id), y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +                 # Use columns to represent sentiment scores
  facet_wrap(~method, ncol = 1, scales = "free_y") +  # Facet by sentiment method
  labs(title = "Sentiment Analysis by Lexicon",
       x = "Subreddit post (post id)",
       y = "Net Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),            # Hide x-axis labels for clarity
        axis.ticks.x = element_blank())           # Remove x-axis ticks

# All three lexicons agree on the overall trends in sentiment.
# The sentiment scores are relatively balanced, with a mix of positive and negative sentiments

#### Most Common Positive and Negative Words ####
# Identify words that contribute most to positive and negative sentiment

# Identify words that contribute most to positive and negative sentiment
bing_word_counts <- tokenized_comments %>%
  inner_join(bing, by = "word") %>%         # Join with Bing lexicon
  count(word, sentiment, sort = TRUE) %>%  # Count word occurrences by sentiment
  ungroup()

# View the most common positive and negative words
print(head(bing_word_counts, 10))

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

# The word "like" may be incorrectly influencing sentiment analysis:
# - "like" is classified as positive in the Bing lexicon.
# - However, "like" is often used in a neutral context, such as "I like apples."

# To address this issue, we can create a custom stop word list to exclude "like" 
# from the analysis.

# Create a custom stop word list to exclude "patient"
custom_stop_words <- bind_rows(
  tibble(word = c("like"), lexicon = c("custom")),  # Add "like" as a custom stop word
  stop_words                                               # Combine with the standard stop word list
)

#### Creating Word Clouds ####
# Visualize the most common words using word clouds

# Remove stop words
tidy_abstracts_no_stop <- tokenized_comments %>%
  anti_join(stop_words, by = "word")  # Exclude stop words

# Create a word cloud of the most common words
tidy_abstracts_no_stop %>%
  count(word) %>%                     # Count word occurrences
  with(wordcloud(word, n, max.words = 100))  # Generate a word cloud




### Tokenize the post body also ###


