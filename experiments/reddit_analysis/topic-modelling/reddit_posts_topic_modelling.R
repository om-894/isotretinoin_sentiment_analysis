
library(tidytext)     # For text mining with tidy data principles
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(tidyr)        # For data tidying
library(ggplot2)      # For data visualization
library(wordcloud)    # For creating word clouds
library(reshape2)     # For reshaping data
library(scales)       # For scaling in plots
library(readr)        # For reading data
library(topicmodels)  # For topic modeling

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

# get the Reddit posts and comments dataset

reddit_data <- read_csv("data-raw/reddit-posts-and-comments/all_subreddits_reddit_posts.csv")


# I want to also keep the title of the post but just combine the comments. 
# Combine comments for each post_id and keep the post title and text
df_combined <- reddit_data %>%
  group_by(subreddit, post_id, post_title, post_body) %>%  # Keep post id, title and text
  summarise(comments_combined = paste(comment, collapse = " "), .groups = "drop")


# Tokenize the text and remove stopwords ---------------------------------------

df_tokens <- df_combined %>%
  mutate(full_text = paste(post_title, post_body, comments_combined, sep = " ")) %>%
  unnest_tokens(word, full_text) %>%  # Tokenize the text
  anti_join(stop_words, by = "word") %>%  # Remove stopwords
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(nchar(word) > 2)
  
# Create a Document-Term Matrix (DTM) ------------------------------------------

df_dtm <- df_tokens %>%
  count(post_id, word, sort = TRUE) %>%
  cast_dtm(document = post_id, term = word, value = n)

# Fit an LDA model -------------------------------------------------------------

# Use LDA() function from topicmodels package, setting k = 5 to create a 2-topic LDA model
# set a seed so that the output of the model is predictable
reddit_lda <- LDA(df_dtm, k = 3, control = list(seed = 1234))
reddit_lda

# Notes that fitting the model is the easy part - now need to explore and interpret the 
## model using the tidy approach

# Examine Topic-Term Probability (Beta) ----------------------------------------

# Use tidy() from tidytext to extract per-topic-per-word probabilities from the model
reddit_topics <- tidy(reddit_lda, matrix = "beta")

head(reddit_topics)

# For each topic-term combination, the model copmutes the probability of that term being
## generated from that topic.

# Can use dplyr's top_n() to find the 10 terms that are most common within each topic and
## then visualize with ggplot2

reddit_top_terms <- reddit_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

reddit_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%  # Reorder terms within each topic 
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms in Each Topic",
    x = NULL,
    y = expression(beta)
  )

# Topic 1 (Red): Appears to be about personal experiences with Accutane treatment, 
# containing terms like "accutane," "skin," "acne," "amazing," and emotional 
# expressions like "happy" and "wow."

# Topic 2 (Green): Seems to focus on medical/technical aspects of treatment, with 
# terms like "lithium," "isotretinoin" (which is the generic name for Accutane), 
# "beta," "treatment," and other medical terminology.

# Topic 3 (Blue): Appears to be about side effects and treatment outcomes, with 
# terms like "accutane," "effects," "drug," "pain," and "issues."

# Looking at the grpahs, i need to make a custom stop word list to get rid of 
# words like 'it's', 'https', 'i'm' etc..




# Ideas:
# Four topic model for comments
# Two topic model for posts - compare topics between posts and comments
# Three topic model for 



