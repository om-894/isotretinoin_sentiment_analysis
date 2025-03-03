
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
  labs(x = "Rank", y = "Term Frequency", title = "Zipf's Law in Jane Austen's Novels") +
  theme_minimal()

# The plot shows a linear relationship on a log-log scale,
# indicating that Zipf's Law holds for these novels.



