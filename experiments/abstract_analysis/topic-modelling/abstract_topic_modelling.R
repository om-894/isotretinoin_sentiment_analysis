
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

# Load the abstract data
data <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")

# Filter rows where `has_abstract` is TRUE and remove missing abstracts
abstracts_data <- data %>%
  filter(has_abstract == TRUE, !is.na(abstract)) %>%
  select(pmid, title, abstract, year)

# SPLIT DATAFRAME WHEN CERTAIN MEASURES WERE INTRODUCED???

# IDEA: split abstracts at 2006 because reddit founded late 2005 and iPLEDGE was introduced 
# in 2006.This woulkd address the question of whether the introduction of iPLEDGE had an 
# impact on the number of articles published on isotretinoin and a fair timeframe comparison 
# with Reddit.
# Hypothesis: would expect a rise in words such as pregnancy, teratogenic, iPLEDGE, etc.
# Hypothesis: More overall worry and bad stigma about accutane/isotretinoin in socail media 
# posts compared to papers post-2006. Does social media affect papers before and after 2006?

# Split the data into two data frames based on the year
abstracts_data_pre_2006 <- abstracts_data %>%
  filter(year <= 2006)

abstracts_data_post_2006 <- abstracts_data %>%
  filter(year > 2006)

# Make sure to still analyse all 3 dataframes. MOVE THIS TO AFTER TOKENIZATION


# Tokenize the text and remove stopwords from both dataframes ------------------

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
df_tokens <- abstracts_data %>%
  mutate(full_text = paste(title, abstract, sep = " ")) %>%
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
  count(title, word, sort = TRUE) %>%
  cast_dtm(document = title, term = word, value = n)


# Fit an LDA model -------------------------------------------------------------

# Use LDA() function from topicmodels package, setting k = 4 to create a 4-topic LDA model
# set a seed so that the output of the model is predictable and the same seed as reddit analysis
# This will take a few seconds to run
abstract_lda <- LDA(df_dtm, k = 4, control = list(seed = 1234))

# Notes that fitting the model is the easy part - now need to explore and interpret the 
## model using the tidy approach

# Examine Topic-Term Probability (Beta) ----------------------------------------
  
# Use tidy() from tidytext to extract per-topic-per-word probabilities from the model
abstract_topics <- tidy(abstract_lda, matrix = "beta")


# For each topic-term combination, the model copmutes the probability of that term being
## generated from that topic.

# Can use dplyr's top_n() to find the 10 terms that are most common within each topic and
## then visualize with ggplot2

abstract_top_terms <- abstract_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Create interactive visualization of top terms
abstract_top_terms %>%
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
    axis.title.x = element_text(size = 18),  # Change x-axis label size
    axis.title.y = element_text(size = 18),   # Change y-axis label size
    axis.ticks.length = unit(5, "pt"), # Adjust tick length
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1), # Black outline for facet labels
    axis.text.x = element_text(size = 18),  # Increased x-axis text size
    axis.text.y = element_text(size = 18),  # Increased y-axis text size
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10) # Adjust margins (top, right, bottom, left)
  )

# save the figure
ggsave("figures/abstract_figures/topic_modeling_figures/top_terms_per_topic_abstracts.png",
       width = 10, height = 8, dpi = 600, bg = "white")


# Analyze document-topic probabilities (Gamma) ---------------------------------

doc_topics <- tidy(abstract_lda, matrix = "gamma")

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

# The similar distribution patterns across all four topics suggests relatively 
# balanced topic assignments in the corpus


### Topic correlation analysis -------------------------------------------------

topic_correlations <- doc_topics %>%
  spread(topic, gamma) %>%
  select(-document) %>%
  cor()

# Create a PNG device before plotting
png("figures/abstract_figures/topic_modeling_figures/topic_correlations_abstracts.png",
    width = 800, height = 800, res = 150)

# Visualize topic correlations
corrplot(topic_correlations, method = "color",
         type = "upper", order = "hclust",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         diag = FALSE)

# Close the device to save the file
dev.off()

