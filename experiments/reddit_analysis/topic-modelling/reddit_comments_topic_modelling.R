
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
library(corrplot)     # For correlation plots
library(plotly)       # For interactive plots

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

# Additional preprocessing
df_tokens <- df_combined %>%
  mutate(full_text = paste(post_title, post_body, comments_combined, sep = " ")) %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(nchar(word) > 2) %>%
  # Add these steps:
  filter(!word %in% c("amp", "http", "https", "com", "www")) %>% # Remove common web artifacts
  filter(!str_detect(word, "^[[:punct:]]+$")) %>% # Remove punctuation-only tokens
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) # Clean remaining punctuation


# Create custom stop words list
custom_stop_words <- c(
  "it's", "i'm", "don't", "that's", "i've", "i'll", "can't", "won't",
  "https", "http", "amp", "com", "www",
  "im", "ive", "id", "ill", "dont", "cant", "wont", "thats",
  "deleted", "removed", "edit", "edited",
  "like", "just", "really", "get", "got", "going", "went",
  "will", "would", "could", "should", "may", "might",
  "one", "two", "three", "first", "second", "third",
  "way", "thing", "things", "something", "anything",
  "much", "many", "lot", "lots",
  "said", "say", "says", "saying",
  "know", "think", "thought", "thinking",
  "even", "still", "also", "else",
  "new", "old", "since", "ago",
  "day", "days", "week", "weeks", "month", "months",
  "want", "wanted", "wanting",
  "make", "made", "making",
  "use", "used", "using", "feel", "its", "pas", "taking"
)

# Combine with built-in stop words
all_stop_words <- bind_rows(
  stop_words,
  data.frame(word = custom_stop_words, lexicon = "custom")
)

# Update your tokenization code
df_tokens <- df_combined %>%
  mutate(full_text = paste(post_title, post_body, comments_combined, sep = " ")) %>%
  unnest_tokens(word, full_text) %>%
  anti_join(all_stop_words, by = "word") %>%  # Use the combined stop words
  filter(!str_detect(word, "^[0-9]+$")) %>%  # Remove numbers
  filter(nchar(word) > 2) %>%  # Remove short words
  filter(!str_detect(word, "^[[:punct:]]+$")) %>%  # Remove punctuation-only tokens
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>%  # Clean remaining punctuation
  filter(!str_detect(word, "^.*\\d+.*$")) %>%  # Remove tokens containing numbers
  filter(!str_detect(word, "^[a-z]{1,2}$"))    # Remove 1-2 letter words

  
# Create a Document-Term Matrix (DTM) ------------------------------------------

df_dtm <- df_tokens %>%
  count(post_id, word, sort = TRUE) %>%
  cast_dtm(document = post_id, term = word, value = n)

# Assess different numbers of topics -------------------------------------------

# Try different numbers of topics for model selection
k_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)
perplexities <- data.frame(k = k_values, perplexity = NA)

for(i in seq_along(k_values)) {
  model <- LDA(df_dtm, k = k_values[i], control = list(seed = 1234))
  perplexities$perplexity[i] <- perplexity(model)
}

# Plot perplexity scores
ggplot(perplexities, aes(x = k, y = perplexity)) +
  geom_line() +
  geom_point() +
  labs(title = "Model Perplexity by Number of Topics",
       x = "Number of Topics (k)",
       y = "Perplexity")

# Based on perplexity scores, the model with 3 topics seems to be the best choice
# While the perplexity continues to decrease after 5 topics, the rate of improvement 
# becomes much more gradual

# We will assess the differences between using 4 and 5 topics.

# Fit an LDA model -------------------------------------------------------------

# Use LDA() function from topicmodels package, setting k = 3 to create a 3-topic LDA model
# set a seed so that the output of the model is predictable
reddit_lda <- LDA(df_dtm, k = 4, control = list(seed = 1234))

# Notes that fitting the model is the easy part - now need to explore and interpret the 
## model using the tidy approach

# Examine Topic-Term Probability (Beta) ----------------------------------------

# Use tidy() from tidytext to extract per-topic-per-word probabilities from the model
reddit_topics <- tidy(reddit_lda, matrix = "beta")


# For each topic-term combination, the model copmutes the probability of that term being
## generated from that topic.

# Can use dplyr's top_n() to find the 10 terms that are most common within each topic and
## then visualize with ggplot2

reddit_top_terms <- reddit_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Create interactive visualization of top terms
reddit_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic), 
             text = paste("Term:", term, "\nBeta:", round(beta, 4)))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "term",
    y = expression(beta)
  ) +
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

# save the figure
ggsave("figures/reddit_figures/topic_modeling_figures/top_terms_per_topic.png")


# Analyze document-topic probabilities (Gamma) ---------------------------------

doc_topics <- tidy(reddit_lda, matrix = "gamma")

# Document-topic distribution summary
doc_topic_distribution <- doc_topics %>%
  group_by(topic) %>%
  summarise(mean_gamma = mean(gamma),
            sd_gamma = sd(gamma)) %>%
  arrange(desc(mean_gamma))

# Visualize document-topic distribution
ggplot(doc_topics, aes(gamma)) +
  geom_histogram(bins = 50) +
  facet_wrap(~topic, ncol = 2) +
  labs(title = "Distribution of document probabilities for each topic",
       x = "Probability (gamma)",
       y = "Count")

# The bimodal distribution in each panel shows that documents tend to either strongly 
# belong (probability near 1.0) or strongly not belong (probability near 0.0) to each topic

# The high counts at probability near 0.0 indicate that most documents have low 
# probability of belonging to any single topic. The smaller peaks at probability 1.0 show 
# that some documents are strongly associated with each topic

# This pattern suggests clear topic separation, where documents tend to be distinctly 
# categorized rather than having mixed topic assignments

# The similar distribution patterns across all three topics suggests relatively 
# balanced topic assignments in the corpus

# Topic correlation analysis ---------------------------------------------------

topic_correlations <- doc_topics %>%
  spread(topic, gamma) %>%
  select(-document) %>%
  cor()

# Visualize topic correlations
corrplot(topic_correlations, method = "color",
         type = "upper", order = "hclust",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         diag = FALSE)

# save the figure
ggsave("figures/reddit_figures/topic_modeling_figures/topic_correlations_comments.png")

# The negative correlations across all topic pairs suggest that these topics are 
# relatively distinct from each other, which is desirable in topic modeling.

# The strongest distinction is between Topics 1 and 3 (-0.70), indicating these 
# topics are most different from each other. Topics 1 and 2 show moderate distinction (-0.43)
# Topics 2 and 3 have the weakest negative correlation (-0.35), suggesting these topics 
# might have some overlap in content



