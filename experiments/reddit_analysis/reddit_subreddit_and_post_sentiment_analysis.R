
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

# View the result
head(df_posts)

# So far, i have the sentiment score for each individual post. Each post had a combined
# sentiment score. I would now like to see the total sentiment for each subreddit,
# based on the sentiment of the posts in that subreddit only.

# Later i will make a script to create a dataframe that combines the posts comments 
# sentiment scores with the posts themselves. This will allow me to see the overall 
# sentiment of the posts and subreddits and assess if comments match to posts.

#############################################################################
#       Tokenize the posts into words and perform sentiment analysis     #
#############################################################################

# Tokenize the posts into words
tokenized_posts <- df_posts %>%
  unnest_tokens(output = word, input = post_body)  # Tokenize the comments into words

# View the tokenized data
print(head(tokenized_posts))

### NRC lexicon sentiment analysis ###
sentiment_nrc <- tokenized_posts %>%
  inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")), by = "word") %>%
  mutate(method = "NRC")

# Filter the NRC lexicon for words associated with "fear"
nrc_fear <- nrc %>%
  filter(sentiment == "fear")

# Find the most common "fear" words in the comments
fear_words <- tokenized_posts %>%
  inner_join(nrc_fear, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)              # Count occurrences

# View the most common fear words
print(head(fear_words, 10))

# These words show a negative sentiment. The words are associated with fear and negative emotions.


### Bing lexicon sentiment analysis ###
# Assign sentiment to words using the Bing lexicon
sentiment_bing <- tokenized_posts %>%
  inner_join(bing, by = "word") %>%
  mutate(method = "BING")

# Count positive and negative words for each post
post_sentiment <- sentiment_bing %>%
  group_by(subreddit, post_id, post_title) %>%  # Group by post (identified by post_title)
  count(sentiment) %>%                # Count positive and negative words
  spread(sentiment, n, fill = 0) %>%  # Convert to wide format
  mutate(sentiment = positive - negative)  # Calculate net sentiment

# View sentiment scores for each posts' comments
print(head(post_sentiment))

# Looks interesting. The negative posts are definately showing the lowest sentiment scores.

# Filter the posts with the most negative sentiments in decending order
most_negative <- post_sentiment %>%
  arrange(sentiment) %>%  # Sort by sentiment
  select(subreddit, post_id, post_title, sentiment)  # Select the post_id, post_title and sentiment

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
  select(subreddit, post_id, post_title, sentiment)  # Select the PMID and sentiment

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
sentiment_afinn <- tokenized_posts %>%
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




