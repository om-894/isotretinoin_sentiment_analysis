
library(tidytext)     # For text mining with tidy data principles
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(tidyr)        # For data tidying
library(ggplot2)      # For data visualization
library(wordcloud)    # For creating word clouds
library(reshape2)     # For reshaping data
library(scales)       # For scaling in plots
library(readr)        # For reading data

### Exploring Sentiment Lexicons------------------------------------------------
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


### Perform Sentiment Analysis on the comments and posts from reddit------------
# data was gathered using python script 'subs_and_comments_script.py'
# We will tokenize the articles into words and perform sentiment analysis.

# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- data %>%
  group_by(subreddit, post_id, post_title, post_body) %>%  # Keep post id, title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# View the result
head(df_combined)

# Looks great. I now need to drop posts with no comments, since they wont be of as 
# much use as pots with comments
df_combined <- df_combined %>%
  filter(comments_combined != "No comments")


### Tokenize the comments into words and perform sentiment analysis-------------

# Tokenize the comments into words
tokenized_comments <- df_combined %>%
  unnest_tokens(output = word, input = comments_combined)  # Tokenize the comments into words

# View the tokenized data
print(head(tokenized_comments))


### NRC lexicon sentiment analysis----------------------------------------------
sentiment_nrc <- tokenized_comments %>%
  inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")), by = "word") %>%
  mutate(method = "NRC")

# Filter the NRC lexicon for words associated with "anger"
nrc_anger <- nrc %>%
  filter(sentiment == "anger")

# Filter the NRC lexicon for words associated with "joy"
nrc_joy <- nrc %>%
  filter(sentiment == "joy")

# Find the most common "joy" words in the comments
joy_words <- tokenized_comments %>%
  inner_join(nrc_joy, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)              # Count occurrences

# View the most common joy words
print(head(joy_words, 10))

# plot the most common joy words
joy_words %>%
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
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Save to figures folder
# ggsave("figures/reddit_figures/reddit_comments_joy_words.png")

# Find the most common "anger" words in the comments
anger_words <- tokenized_comments %>%
  inner_join(nrc_anger, by = "word") %>%  # Join with fear words
  count(word, sort = TRUE)                # Count occurrences

# View the most common anger words
print(head(anger_words, 10))

# plot the most common anger words
anger_words %>%
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
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Save to figures folder
# ggsave("figures/reddit_figures/reddit_comments_anger_words.png")


### Bing lexicon sentiment analysis---------------------------------------------

# Assign sentiment to words using the Bing lexicon
sentiment_bing <- tokenized_comments %>%
  inner_join(bing, by = "word") %>%
  mutate(method = "BING")

# Count positive and negative words for each abstract
post_comment_sentiment_bing <- sentiment_bing %>%
  group_by(subreddit, post_id, post_title, post_body) %>%                  # Group by post (identified by post_title)
  count(sentiment) %>%                # Count positive and negative words
  spread(sentiment, n, fill = 0) %>%  # Convert to wide format
  mutate(sentiment = positive - negative)  # Calculate net sentiment


# Filter the posts with the most negative sentiments in decending order
most_negative <- post_comment_sentiment_bing %>%
  arrange(sentiment) %>%  # Sort by sentiment
  select(post_id, post_title, sentiment)  # Select the post_id, post_title and sentiment

# shorten post title so it fits on the graph
most_negative %>%
  head(10) %>%  # Take the 10 most negative abstracts
  ggplot(aes(x = reorder(as.factor(post_id), -sentiment), y = sentiment)) +
  geom_col(fill = "indianred2", color = "indianred2") +  # Red bars with black outlines
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


### Analyzing Units Beyond Just Words------------------------------------------
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


### Most Common Positive and Negative Words for BING----------------------------

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
  

# The word "like" may be incorrectly influencing sentiment analysis:
# - "like" is classified as positive in the Bing lexicon.
# - However, "like" is often used in a neutral context, such as "I like apples."

# To address this issue, we can create a custom stop word list to exclude "like" 
# from the analysis.

# Custom stop words
custom_swear_words <- tibble(
  word = c("bitch", "bitches", "cunt", "bastard", "shit", "fucking", "fuck", 
           "ass", "fucked", "bullshit", "dick", "wtf", "asshole", "piss", "scumbag",
           "fucker", "fuckers"),
  lexicon = "custom"
)

# Combine custom and standard stop words
custom_stop_words <- bind_rows(
  custom_swear_words,
  stop_words
)

# Add "like" as a custom stop word
custom_stop_words <- bind_rows(
  tibble(word = c("like"), lexicon = c("custom")),
  custom_stop_words
)

# View the custom stop words
print(custom_stop_words)

# Tokenize the comments into words, excluding custom stop words
tokenized_comments_custom <- df_combined %>%
  unnest_tokens(output = word, input = comments_combined, token = "regex", pattern = "\\s+") %>%
  anti_join(custom_stop_words, by = "word")  # Exclude custom stop words

# Count the most common positive and negative words
bing_word_counts_custom <- tokenized_comments_custom %>%
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
# ggsave("figures/reddit_figures/reddit_comments_bing_overall_sentiment.png")


### AFINN lexicon sentiment analysis--------------------------------------------

# Calculate word frequency and sentiment value
sentiment_afinn <- tokenized_comments %>%
  inner_join(afinn, by = "word") %>%
  mutate(method = "AFINN") %>%
  count(word, value, sort = TRUE)

# Without swear words
# sentiment_afinn <- tokenized_comments_custom %>%
#   inner_join(afinn, by = "word") %>%
#   mutate(method = "AFINN") %>%
#   count(word, value, sort = TRUE)

# Filter top 10 words for each sentiment value and create the plot
sentiment_afinn %>%
  group_by(value) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = n, y = reorder(word, n), fill = value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ value, scales = "free_y") +
  labs(x = "Contribution to Sentiment", y = NULL) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10)) + 
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(3, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    strip.text = element_text(size = 9),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Save the plot
# ggsave("figures/reddit_figures/reddit_comments_afinn_sentiment_grades_with_swear.png")


### Group posts by subreddit to get overall subreddit sentiment with BING-------

# Assuming post_sentiment is already calculated
# Select top 10 posts by sentiment for each subreddit
top_posts_bing <- post_comment_sentiment_bing %>%
  group_by(subreddit) %>%
  top_n(10, wt = abs(sentiment)) %>%  # Top 10 by absolute sentiment score
  ungroup()

# Plot the data
ggplot(top_posts_bing, aes(x = reorder(post_id, sentiment), y = sentiment, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ subreddit, scales = "free_y", ncol = 2) +  # Adjust ncol for layout
  labs(x = NULL, y = "Sentiment Score") +
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
# ggsave("figures/reddit_figures/reddit_subreddits_top_sentiments_comments_bing.png")


### Group posts by subreddit to get overall subreddit sentiment with AFINN------

# count afinn sentiment for each comment
post_comment_sentiment_afinn <- tokenized_comments %>%
  inner_join(afinn, by = "word") %>%
  group_by(subreddit, post_id, post_title) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# get top 10 posts by sentiment for each subreddit
top_posts_afinn <- post_comment_sentiment_afinn %>%
  group_by(subreddit) %>%
  top_n(10, wt = abs(sentiment)) %>%  # Top 10 by absolute sentiment score
  ungroup()

# Plot the data
ggplot(top_posts_afinn, aes(x = reorder(post_id, sentiment), y = sentiment, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ subreddit, scales = "free_y", ncol = 2) +  # Adjust ncol for layout
  labs(x = NULL, y = "Sentiment Score") +
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
# ggsave("figures/reddit_figures/sentiment_analysis_figures/reddit_subreddits_top_sentiments_comments_afinn.png")


