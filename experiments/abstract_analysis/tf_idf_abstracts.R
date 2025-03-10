
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
  theme_minimal()

# The line of best fit in your graph shows that the term frequencies generally follow 
# Zipf's Law, with a consistent inverse relationship between rank and frequency across 
# both time periods, though there may be slight variations in the distribution.








