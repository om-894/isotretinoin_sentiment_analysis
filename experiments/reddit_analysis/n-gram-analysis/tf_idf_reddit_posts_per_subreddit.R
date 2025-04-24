
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

# Tokenize the posts into words
post_words <- df_posts %>%
  group_by(subreddit) %>%
  unnest_tokens(word, post_body) %>%  # Assuming the column is named 'comment_text'
  count(word, sort = TRUE) %>%  # Count word frequencies
  ungroup()

# Analyzing the term frequency -------------------------------------------------

# Calculate the total number of words in each subreddit
total_subreddit_words <- post_words %>%
  group_by(subreddit) %>%
  summarize(total = sum(n))

# Join the total word counts to the word counts data frame
post_words <- left_join(post_words, total_subreddit_words, by = "subreddit")


# Zipf's Law -------------------------------------------------------------------

# Zipf's Law states that the frequency of a word is inversely proportional to its rank in the frequency table.
# We can explore this relationship by plotting the term frequency versus rank on a log-log scale.

# Calculate rank and term frequency
freq_by_rank <- post_words %>%
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
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.title.x = element_text(size = 18),  # Change x-axis label size
    axis.title.y = element_text(size = 18),   # Change y-axis label size
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    axis.text.x = element_text(size = 18),  # Increased x-axis text size
    axis.text.y = element_text(size = 18),  # Increased y-axis text size
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10), # Adjust margins (top, right, bottom, left)
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1.5, "cm")
  )

# The fitted line has a slope close to -1, consistent with Zipf's Law,
# which states that term frequency is inversely proportional to rank.

# Save the figure
ggsave("figures/reddit_figures/n_gram_and_term_frequency_figures/zip_f_posts.png",
       width = 13, height = 9, dpi = 600, bg = "white")

# Calculating tf-idf -----------------------------------------------------------

# The tf-idf statistic reflects how important a word is to a document in a collection.
# We can calculate tf-idf to find words that are important to each novel.

# Use bind_tf_idf() to calculate tf, idf, and tf-idf
post_words <- post_words %>%
  bind_tf_idf(word, subreddit, n)


# Exploring High tf-idf Words --------------------------------------------------

# Let's look at the words with the highest tf-idf scores in each subreddit.

# View the words with highest tf-idf across all books
post_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# We can visualize the top 15 words with the highest tf-idf for each book.

post_words %>%
  group_by(subreddit) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n = 10) %>%
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

# Removing Less Meaningful Words -----------------------------------------------

# We notice that some words like "et", "al", "pubmed.ncbi.nlm.nih.gov" are artifacts 
# or less meaningful.
# Let's remove these words using a custom stop words list.

# Create a custom stop words list
custom_stop_words <- tibble(word = c("drive.google.com", "elta", "ms", "et", "al", 
                                     "pubmed.ncbi.nlm.nih.gov", "ci", "uc", 
                                     "www.accessdata.fda.gov", "youuu", "https", "170"))

# Remove custom stop words from the data
post_words_clean <- post_words %>%
  anti_join(custom_stop_words, by = "word")

# Recalculate tf-idf
post_words_clean <- post_words_clean %>%
  bind_tf_idf(word, subreddit, n)

post_words_clean %>%
  group_by(subreddit) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, subreddit)) %>%  # Reorder words within each book
  ggplot(aes(word, tf_idf, fill = subreddit)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Unigram", y = "tf-idf") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    axis.ticks.y = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.x = element_line(color = "black"), # Add tick marks to y-axis
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    axis.text.x = element_text(size = 12),  # Increased x-axis text size
    axis.text.y = element_text(size = 12),  # Increased y-axis text size
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# save the figure
ggsave("figures/reddit_figures/n_gram_and_term_frequency_figures/tf_idf_unigrams_posts.png",
       width = 10, height = 8, dpi = 600, bg = "white")






