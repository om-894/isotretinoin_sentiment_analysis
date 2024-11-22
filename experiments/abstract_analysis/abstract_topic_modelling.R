
############

# Chapter 6 - Topic Modeling

############

# Topic modeling is a method of unsupervised classification of documents, similar to 
## clustering, which finds natural groups of items. Latent Dirichlet allocation (LDA)
## is a popular method for fitting topic models. Treats each document as a mixture
## of topics and each topic as a mixture of words. Allows documents to overlap each other
## in terms of content, rather than being separated into discrete groups.

# Latent Dirichlet allocation ---------------------------------------------

# Two main principles
# Every document is a mixture of topics: e.g., "Document 1 is 90% topic A and 10%
## topic B, while Document 2 is 30% topic A and 70% topic B

# Every topic is a mixture of words: e.g., two-topic model of politics and entertainment
## may have president, congress, and government as common words in politics
## and movies, television, and actor in entertainment, but budget in both

# get the AP dataset

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# Use LDA() function from topicmodels package, setting k = 2 to create a 2-topic LDA model
# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# Notes that fitting the model is the easy part - now need to explore and interpret the 
## model using the tidy approach

# Word-topic probabilities ------------------------------------------------

# Use tidy() from tidytext to extract per-topic-per-word probabilities from the model

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# For each topic-term combination, the model copmutes the probability of that term being
## generated from that topic.

# Can use dplyr's top_n() to find the 10 terms that are most common within each topic and
## then visualize with ggplot2

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Looks like business and politics

# Alternative - look at terms that had the greatest difference in beta between the
## two topics using the log ratio (helpful because it makes the difference
## symmetrical). Also limit it to a set of the most common words (beta > 1/1000)

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

## # A tibble: 198 × 4
##              term       topic1       topic2   log_ratio
##             <chr>        <dbl>        <dbl>       <dbl>
## 1  administration 4.309502e-04 1.382244e-03   1.6814189
## 2             ago 1.065216e-03 8.421279e-04  -0.3390353
## 3       agreement 6.714984e-04 1.039024e-03   0.6297728
## 4             aid 4.759043e-05 1.045958e-03   4.4580091
## 5             air 2.136933e-03 2.966593e-04  -2.8486628
## 6        american 2.030497e-03 1.683884e-03  -0.2700405
## 7        analysts 1.087581e-03 5.779708e-07 -10.8778386
## 8            area 1.371397e-03 2.310280e-04  -2.5695069
## 9            army 2.622192e-04 1.048089e-03   1.9989152
## 10          asked 1.885803e-04 1.559209e-03   3.0475641
## # ... with 188 more rows

beta_spread %>% 
  mutate(abs_log = abs(log_ratio)) %>% 
  mutate(rank = dense_rank(desc(abs_log))) %>% 
  filter(rank <= 20) %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio)) + 
  geom_bar(stat = "identity") +
  ylab("Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

# Figure 6-3. Words with the greatest difference in β between topic 2 and topic 1

# Document-topic probabilities --------------------------------------------

# Examing per-document-per-topic probabilities, called "gamma"
## Each value is an estimated proportion of words from that document that are generated
## from that topic.

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

## # A tibble: 4,492 × 3
##    document topic        gamma
##       <int> <int>        <dbl>
## 1         1     1 0.2480616686
## 2         2     1 0.3615485445
## 3         3     1 0.5265844180
## 4         4     1 0.3566530023
## 5         5     1 0.1812766762
## 6         6     1 0.0005883388
## 7         7     1 0.7734215655
## 8         8     1 0.0044516994
## 9         9     1 0.9669915139
## 10       10     1 0.1468904793
## # ... with 4,482 more rows

# Looks like most documents are a mix of topics but doc 6 is almost exclusively
## topic 2. Check this by looking at most common words in that doc

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

# Example: the great library heist ----------------------------------------
## Four books - chapters torn apart, unlabeled, and placed in a pile
## Use topic modeling to see how the chapters cluster - should tell us
## which chapters belongs to which book

# Set a different mirror
options(gutenberg.mirror = "http://www.gutenberg.org")

# Define the Gutenberg IDs for the books
book_ids <- c(36, 158, 74, 59)

# Download the books using their Gutenberg IDs
books <- gutenberg_download(book_ids, meta_fields = "title")

# Check the downloaded books
unique(books$title)

# Tokenize into separate words, remove stop words, and treat each chapter as sep document

library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# LDA on chapters ---------------------------------------------------------

# topicmodels package requires DocumentTermMatrix, so convert tidy to DTM

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

# Use LDA to create a four-topic model
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

# Check per-topic-per-word probabilities
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# Sample rows explained:
# 1     1     tom       2.18e-184   # The term "tom" has a very low probability (2.18e-184) of being in topic 1.
# 2     2     tom       2.97e-  2   # The term "tom" has a higher probability (2.97e-2) of being in topic 2.
# 3     3     tom       1.65e-  2   # Probability of "tom" in topic 3.
# 4     4     tom       2.11e-  7   # Probability of "tom" in topic 4.
# 5     1     harriet   1.32e-  2   # Probability of "harriet" in topic 1.
# 6     2     harriet   1.91e-204   # Very low probability of "harriet" in topic 2.
# 7     3     harriet   2.70e- 12   # Probability of "harriet" in topic 3.
# 8     4     harriet   3.56e-  3   # Probability of "harriet" in topic 4.
# 9     1     miss      1.11e-  2   # Probability of "miss" in topic 1.
# 10    2     miss      3.38e-  4   # Probability of "miss" in topic 2.

# top five terms in each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Visualize
library(ggplot2)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Per-document classification ---------------------------------------------
# Try to put the chapters back in the right books by looking at gamma 
## - the per-document-per-topic probabilities

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

# For example, the model estimates that each word in the Great Expectations_57 document 
# has only a 0.00135% probability of coming from topic 1 (Pride and Prejudice

# Would expect chapters within a book to be mostly generated from the corresponding topic

# Separate doc name into title and chapter
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

# Check for cases where topic most associated w/ a chapter belonged to another book
# First find topic most associated with each chapter using top_n()
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

# Sample rows explained:
# 1     Emma       47     1     0.877   # Chapter 47 of "Emma" has a high association (0.877) with topic 1.
# 2     Emma       21     1     0.816   # Chapter 21 has an 81.6% probability of being generated from topic 1.
# 3     Emma       26     1     0.614   # Chapter 26 has a lower association (0.614) with topic 1.
# 4     Emma       46     1     1.00    # Chapter 46 is fully associated with topic 1.
# 5     Emma        7     1     1.00    # Chapter 7 is also fully associated with topic 1.
# 6     Emma        1     1     1.00    # Chapter 1 is entirely associated with topic 1.
# 7     Emma       49     1     0.820   # Chapter 49 has an 82% association with topic 1.
# 8     Emma        9     1     1.00    # Chapter 9 is fully associated with topic 1.
# 9     Emma        5     1     0.869   # Chapter 5 has an 86.9% association with topic 1.
# 10    Emma        8     1     1.00    # Chapter 8 is fully associated with topic 1.

# Then compare to the "consensus" topic for each book to see which were misidentified
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

# No misclassifications

# By word assignments: augment --------------------------------------------

# Use augment() to identify which words in each document were assigned to which topic
## Augment adds information to each observation in the original data. In this case,
## will add a column (".topic") with the topic each term was assigned to within the doc

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

# Now join with consensus titles to see which were misclassified
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

# Visualize a confusion matrix, showing how often words from one book were assigned
## to another

library(scales)

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

# Figure 6-6. Confusion matrix showing where LDA assigned the words from each book. 
# Each row of this table represents the true book each word came from, and each 
# column represents what book it was assigned to.

# Find most commonly mistaken words
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

# Summary
# This chapter introduced topic modeling for identifying clusters of words that characterize a set of documents.
# We demonstrated how the `tidy()` function allows us to explore and understand these models using `dplyr` and `ggplot2`.
# 
# Key points:
# - The tidy approach simplifies model exploration by handling different output formats through tidying functions.
# - This enables us to examine model results with a consistent set of tools, making analysis and visualization easier.
# - In our example, topic modeling successfully separated and distinguished chapters from four distinct books.
# - We also examined the limitations of the model, identifying words and chapters that were incorrectly assigned by the LDA model.
# 
# This approach highlights the strengths of using a tidy workflow for text mining and topic modeling,
# allowing for efficient model interpretation and insight generation.