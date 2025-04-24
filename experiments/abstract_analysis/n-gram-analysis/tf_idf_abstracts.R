
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
data <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")

# Filter rows where `has_abstract` is TRUE and remove missing abstracts
abstracts_data <- data %>%
  filter(has_abstract == TRUE, !is.na(abstract)) %>%
  select(pmid, title, abstract, year)

abstracts_data <- abstracts_data %>%
  mutate(period = ifelse(year >= 2006, "2006 and later", "Before 2006"))

# Tokenize the text and remove stopwords ---------------------------------------

# Tokenize the abstratcs into words
abstract_words <- abstracts_data %>%
  group_by(period) %>%
  unnest_tokens(word, abstract) %>%  # Assuming the column is named 'comment_text'
  count(word, sort = TRUE) %>%  # Count word frequencies
  ungroup()

# ADD IN REMOVAL OF STOP WORDS #

# Analyzing the term frequency -------------------------------------------------

# Calculate the total number of words in each subreddit
total_abstract_words <- abstract_words %>%
  group_by(period) %>%
  summarize(total = sum(n))

# Join the total word counts to the word counts data frame
abstract_words <- left_join(abstract_words, total_abstract_words, by = "period")

# The data frame 'comment_words' now contains:
# - subreddit: name of the subreddit
# - word: the word
# - n: the number of times the word appears in the subreddit
# - total: the total number of words in the subreddit

# Plotting Term Frequency Distributions ----------------------------------------

# We can visualize the distribution of term frequencies in each novel.
# This shows how often words appear in a novel relative to the total number of words.

ggplot(abstract_words, aes(n / total, fill = period)) +
  geom_histogram(show.legend = FALSE, bins = 30) +
  xlim(NA, 0.0009) +  # Limit x-axis to focus on common term frequencies
  facet_wrap(~period, ncol = 2, scales = "free_y") +
  labs(x = "Term Frequency (n / total words)", y = "Count")

# Zipf's Law -------------------------------------------------------------------

# Zipf's Law states that the frequency of a word is inversely proportional to its rank 
# in the frequency table. We can explore this relationship by plotting the term frequency 
# versus rank on a log-log scale.

# Calculate rank and term frequency
freq_by_rank <- abstract_words %>%
  group_by(period) %>%
  mutate(rank = row_number(),                 # Rank of the word within the book
         term_frequency = n / total) %>%      # Term frequency (proportion)
  ungroup()

# Plot term frequency versus rank on log-log scales
ggplot(freq_by_rank, aes(rank, term_frequency, color = period)) +
  geom_line(linewidth = 1.1, alpha = 0.8) +
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
# Residuals:
#    Min        1Q    Median        3Q       Max 
# -0.101604 -0.011540  0.002634  0.011525  0.110338 

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.139969   0.004469  -255.1   <2e-16 ***
# log10(rank) -0.893053   0.001920  -465.1   <2e-16 ***

# Plot the fitted power law
ggplot(freq_by_rank, aes(rank, term_frequency, color = period)) +
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

# Save the figure
ggsave("figures/abstract_figures/n_gram_and_term_frequency_figures/zip_f_abstracts.png",
       width = 13, height = 9, dpi = 600, bg = "white")

# The line of best fit in your graph shows that the term frequencies generally follow 
# Zipf's Law, with a consistent inverse relationship between rank and frequency across 
# both time periods, though there may be slight variations in the distribution.

# Calculating tf-idf -----------------------------------------------------------

# The tf-idf statistic reflects how important a word is to a document in a collection.
# We can calculate tf-idf to find words that are important to each novel.

# Use bind_tf_idf() to calculate tf, idf, and tf-idf
abstract_words <- abstract_words %>%
  bind_tf_idf(word, period, n)

# View the data frame with tf-idf
abstract_words
# The data frame now includes:
# - tf: term frequency
# - idf: inverse document frequency
# - tf_idf: tf-idf score

# Exploring High tf-idf Words --------------------------------------------------

# Let's look at the words with the highest tf-idf scores in each subreddit.

# View the words with highest tf-idf across all books
abstract_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# We can visualize the top 15 words with the highest tf-idf for each period.

abstract_words %>%
  group_by(period) %>%
  arrange(desc(tf_idf)) %>%
  slice_max(tf_idf, n = 8) %>%  # Get top 8 words per book (doing this so graph looks better)
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, period)) %>%  # Reorder words within each book
  ggplot(aes(word, tf_idf, fill = period)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", title = "Highest tf-idf Words in Jane Austen's Novels") +
  facet_wrap(~period, ncol = 2, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal()

# tf-idf; it identifies words that are important to one document within a 
# collection of documents.

# Removing Less Meaningful Words -----------------------------------------------
  
# We notice that some words like "et", "al", "pubmed.ncbi.nlm.nih.gov" are artifacts 
# or less meaningful.
# Let's remove these words using a custom stop words list.
  
abstract_words_clean <- abstract_words %>%
  filter(
    # Remove URLs and web references
    !str_detect(word, "^https?|www\\.|\\.com|\\.gov|\\.edu|\\.org"),
    # Remove file references, numbers, and alphanumeric codes
    !str_detect(word, "\\d{4}[a-z0-9]+|file\\s|sheet\\s|\\d+\\s*[a-z0-9]+|[a-z0-9]+\\s*\\d+"),
    # Remove academic reference patterns
    !str_detect(word, "article|abstract|doi|pii|^et al|\\sci\\s"),
    # Remove specific patterns you mentioned
    !str_detect(word, "isotretinoin https")
  )

# Create a custom stop words list to remove ones that regex could not remove
custom_stop_words <- tibble(word = c("drive.google.com", "elta", "ms", "et", "al", 
                                       "pubmed.ncbi.nlm.nih.gov", "ci", "uc", 
                                       "www.accessdata.fda.gov", "youuu", "es", "xed",
                                     "itn", "dex", "mcf", "md", "xfc", "der", "hg",
                                     "und"))

# Remove custom stop words from the data
abstract_words_clean <- abstract_words_clean %>%
  anti_join(custom_stop_words, by = "word")

# Recalculate tf-idf
abstract_words_clean <- abstract_words_clean %>%
  bind_tf_idf(word, period, n)

abstract_words_clean %>%
  group_by(period) %>%
  arrange(desc(tf_idf)) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, period)) %>%
  ggplot(aes(word, tf_idf, fill = period)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Unigram", y = "tf-idf") +
  facet_wrap(~period, ncol = 2, scales = "free", labeller = label_both) +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), # Remove gridlines
    axis.line = element_line(color = "black"), # Add black outline to axis
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    axis.text.x = element_text(size = 12),  # Increased x-axis text size
    axis.text.y = element_text(size = 12),  # Increased y-axis text size
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# Save the figure
ggsave("figures/abstract_figures/n_gram_and_term_frequency_figures/tf_idf_unigrams_abstracts.png",
       width = 10, height = 8, dpi = 600, bg = "white")

# Figure should be final

# ipledge YAY!!

# osdi relates to Ocular Surface Disease Index, is a 12-item questionnaire designed 
# to provide a rapid assessment of the symptoms of ocular irritation consistent with 
# dry eye disease and their impact on vision-related functioning.

# CRABP1 is assumed to play an important role in retinoic acid-mediated differentiation 
# and proliferation processes.

# The FTC ensures that isotretinoin's marketing is not deceptive and that pharmaceutical 
# companies engage in fair competition, while the FDA regulates its safety and approval.



