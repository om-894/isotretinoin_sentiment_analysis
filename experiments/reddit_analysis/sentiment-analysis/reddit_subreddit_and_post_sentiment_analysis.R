
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


### Perform Sentiment Analysis on the comments and posts from reddit------------
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


### Tokenize the posts into words and perform sentiment analysis----------------

# Tokenize the posts into words
tokenized_posts <- df_posts %>%
  unnest_tokens(output = word, input = post_body)  # Tokenize the post body into words

# View the tokenized data
print(head(tokenized_posts))


### NRC lexicon sentiment analysis----------------------------------------------

sentiment_nrc <- tokenized_posts %>%
  inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")), by = "word") %>%
  mutate(method = "NRC")

# Filter the NRC lexicon for words associated with "anger"
nrc_anger <- nrc %>%
  filter(sentiment == "anger")

# Filter the NRC lexicon for words associated with "joy"
nrc_joy <- nrc %>%
  filter(sentiment == "joy")

# Find the most common "joy" words in the comments
joy_words <- tokenized_posts %>%
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

# Save to figures folder
# ggsave("figures/reddit_figures/reddit_posts_joy_words.png")

# Find the most common "anger" words in the comments
anger_words <- tokenized_posts %>%
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

# All three lexicons seem to differ. NRC sees one post with a neutral sentiment but 
# BING and AFINN lexicons see it as highly negative
# The sentiment scores are relatively balanced, with a mix of positive and negative sentiments

#### Most Common Positive and Negative Words ####
# Identify words that contribute most to positive and negative sentiment

# Identify words that contribute most to positive and negative sentiment
bing_word_counts <- tokenized_posts %>%
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

# save figure
# ggsave("figures/reddit_posts_common_words.png")

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

#####################################################################
#    Group posts by subreddit to get overall subreddit sentiment    #
#####################################################################

subreddit_sentiment <- post_sentiment %>%
  group_by(subreddit) %>%  # Group by subreddit
  summarise(sentiment = sum(sentiment))  # Sum sentiment scores

# Plot this data
subreddit_sentiment %>%
  ggplot(aes(x = reorder(subreddit, sentiment), y = sentiment)) +
  geom_col(fill = "skyblue", color = "skyblue") +  # Blue bars with blue outlines
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Subreddit Sentiment Scores",
       x = "Subreddit",
       y = "Net Sentiment Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Reduce font size

# save the plot
# ggsave("figures/reddit_subreddit_sentiments.png")


#### Analyzing Units Beyond Just Words ####
# Tokenize text into sentences or chapters

# Tokenize posts into sentences
pandp_sentences <- data_frame(text = pride_prejudice$word) %>%
  unnest_tokens(sentence, text, token = "sentences")

# View a sample sentence
print(pandp_sentences$sentence[2])

# Tokenize the books into chapters using a regex pattern
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%  # Split text at chapter headings
  ungroup()

# Count the number of chapters per book
chapter_counts <- austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

# View the chapter counts
print(chapter_counts)

## # A tibble: 6 × 2
##                  book chapters
##                <fctr>    <int>
## 1 Sense & Sensibility       51
## 2   Pride & Prejudice       62
## 3      Mansfield Park       49
## 4                Emma       56
## 5    Northanger Abbey       32
## 6          Persuasion       25

# Analyze sentiment per chapter
# Get negative words from the Bing lexicon
bing_negative <- bing %>%
  filter(sentiment == "negative")

# Calculate total words per chapter
word_counts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarise(total_words = n())

# Calculate negative words per chapter
negative_counts <- tidy_books %>%
  semi_join(bing_negative, by = "word") %>%  # Keep only negative words
  group_by(book, chapter) %>%
  summarise(negative_words = n())

# Combine word counts and negative counts
negative_ratio <- left_join(negative_counts, word_counts, by = c("book", "chapter")) %>%
  mutate(ratio = negative_words / total_words)  # Calculate ratio of negative words

# Find the chapter with the highest ratio of negative words in each book
highest_negative <- negative_ratio %>%
  filter(chapter != 0) %>%          # Exclude chapters labeled as 0 (e.g., introductions)
  group_by(book) %>%
  top_n(1, ratio) %>%               # Get the chapter with the highest negative ratio
  ungroup()

# View the chapters with the highest negativity
print(highest_negative)

## # A tibble: 6 × 5
##                  book chapter negativewords words      ratio
##                <fctr>   <int>         <int> <int>      <dbl>
## 1 Sense & Sensibility      43           161  3405 0.04728341
## 2   Pride & Prejudice      34           111  2104 0.05275665
## 3      Mansfield Park      46           173  3685 0.04694708
## 4                Emma      15           151  3340 0.04520958
## 5    Northanger Abbey      21           149  2982 0.04996647
## 6          Persuasion       4            62  1807 0.03431101

# What is happening in these chapters? In Chapter 43 of Sense and Sensibility, 
# Marianne is seriously ill, near death; and in Chapter 34 of Pride and Prejudice, 
# Mr. Darcy proposes for the first time (so badly!).

