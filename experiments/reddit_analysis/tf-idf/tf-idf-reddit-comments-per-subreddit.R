
# IF-IDF Explanation
# The TF-IDF score for a term t in a document d within a corpus D is calculated 
# using the following formula:

# TF-IDF(t, d, D) = TF(t, d) × IDF(t, D)

# Term Frequency (TF): Assigns higher weight to terms that appear frequently in a document. 
# If a term appears often in a document, it might be important.

# Inverse Document Frequency (IDF): Assigns higher weight to terms that appear in fewer 
# documents across the corpus. Rare terms across all documents get a higher IDF, 
# highlighting their uniqueness.

# By multiplying TF and IDF, the TF-IDF score balances the frequency of a term in a specific 
# document against its frequency across the entire corpus, effectively highlighting terms 
# that are significant to a particular document.

# HIGH TF-IDF Score: Indicates that the term is very relevant to the document 
# (it appears frequently in that document but not in many documents in the corpus).

# LOW TF-IDF Score: Indicates that the term is common across documents (e.g., common stop words) 
# or does not appear frequently in the document.

# Load necessary libraries
library(tidyverse)    # For data manipulation)
library(dplyr)        # For data manipulation
library(tidytext)     # For text mining with tidy data principles
library(ggplot2)      # For data visualization
library(stringr)      # For string operations


# Load the dataset
data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")

# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- data %>%
  group_by(subreddit, post_id, post_title, post_body) %>%  # Keep post id, title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")

# Tokenize the text and remove stopwords ---------------------------------------

# Tokenize the comments into words
comment_words <- df_combined %>%
  group_by(subreddit) %>%
  unnest_tokens(word, comments_combined) %>%  # Assuming the column is named 'comment_text'
  count(word, sort = TRUE) %>%  # Count word frequencies
  ungroup()

# ADD IN REMOVAL OF STOP WORDS #

# Analyzing the term frequency -------------------------------------------------

# Calculate the total number of words in each subreddit
total_subreddit_words <- comment_words %>%
  group_by(subreddit) %>%
  summarize(total = sum(n))

# Join the total word counts to the word counts data frame
comment_words <- left_join(comment_words, total_subreddit_words, by = "subreddit")

# View the data frame
comment_words
# The data frame 'comment_words' now contains:
# - subreddit: name of the subreddit
# - word: the word
# - n: the number of times the word appears in the subreddit
# - total: the total number of words in the subreddit

# Plotting Term Frequency Distributions ----------------------------------------

# We can visualize the distribution of term frequencies in each novel.
# This shows how often words appear in a novel relative to the total number of words.

ggplot(comment_words, aes(n / total, fill = subreddit)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.0009) +  # Limit x-axis to focus on common term frequencies
  facet_wrap(~subreddit, ncol = 2, scales = "free_y") +
  labs(x = "Term Frequency (n / total words)", y = "Count") +
  ggtitle("Term Frequency Distribution in Isotretinoin related subreddits")

# The single green bar in the AccutaneDamaged subreddit frequency distribution 
# likely indicates that this is a more focused or specialized subreddit where 
# discussions center around a very specific topic - presumably damage or adverse 
# effects from Accutane treatment. Unlike the other subreddits which show more 
# varied term frequencies (multiple bars of different heights), this one appears 
# to have most of its content concentrated around a particular term or set of terms, 
# resulting in that single prominent green bar.

# Zipf's Law -------------------------------------------------------------------

# Zipf's Law states that the frequency of a word is inversely proportional to its rank in the frequency table.
# We can explore this relationship by plotting the term frequency versus rank on a log-log scale.

# Calculate rank and term frequency
freq_by_rank <- comment_words %>%
  group_by(subreddit) %>%
  mutate(rank = row_number(),                 # Rank of the word within the book
         term_frequency = n / total) %>%      # Term frequency (proportion)
  ungroup()

# Plot term frequency versus rank on log-log scales
ggplot(freq_by_rank, aes(rank, term_frequency, color = subreddit)) +
  geom_line(size = 1.1, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Rank", y = "Term Frequency") +
  theme_minimal()

# The plot shows a linear relationship on a log-log scale,
# indicating that Zipf's Law holds for these novels.

# Fitting a Power Law to Zipf's Law --------------------------------------------

# We can fit a linear model to the log-transformed data to find the exponent of the power law.

# Subset the data to exclude extreme ranks
rank_subset <- freq_by_rank %>%
  filter(rank > 10, rank < 500)

# Fit a linear model to the log-transformed term frequency and rank
lm_result <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

# View the results
summary(lm_result)

# Plot the fitted power law
ggplot(freq_by_rank, aes(rank, term_frequency, color = subreddit)) +
  geom_line(size = 1.1, alpha = 0.8) +
  geom_abline(intercept = coef(lm_result)[1], slope = coef(lm_result)[2],
              color = "gray50", linetype = 2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Rank", y = "Term Frequency") +
  theme_minimal()

# The fitted line has a slope close to -1, consistent with Zipf's Law,
# which states that term frequency is inversely proportional to rank.

# Calculating tf-idf -----------------------------------------------------------

# The tf-idf statistic reflects how important a word is to a document in a collection.
# We can calculate tf-idf to find words that are important to each novel.

# Use bind_tf_idf() to calculate tf, idf, and tf-idf
comment_words <- comment_words %>%
  bind_tf_idf(word, subreddit, n)

# View the data frame with tf-idf
comment_words
# The data frame now includes:
# - tf: term frequency
# - idf: inverse document frequency
# - tf_idf: tf-idf score

# Exploring High tf-idf Words --------------------------------------------------

# Let's look at the words with the highest tf-idf scores in each subreddit.

# View the words with highest tf-idf across all books
comment_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# We can visualize the top 15 words with the highest tf-idf for each book.

comment_words %>%
  group_by(subreddit) %>%
  arrange(desc(tf_idf)) %>%
  slice_max(tf_idf, n = 8) %>%  # Get top 8 words per book (doing this so graph looks better)
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, subreddit)) %>%  # Reorder words within each book
  ggplot(aes(word, tf_idf, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", title = "Highest tf-idf Words in Jane Austen's Novels") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal()

# tf-idf; it identifies words that are important to one document within a 
# collection of documents.










