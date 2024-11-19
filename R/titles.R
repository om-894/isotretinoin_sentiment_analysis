# get in the habit of commenting your code restart your R session
# when you begin something new
library(tidyverse)
library(tidytext)
library(wordcloud)


# These abstracts are from with the search
#  '("Isotretinoin"[Mesh] OR Isotretinoin OR Accutane OR Roaccutane)'


# Read vr_treatment data with the extra column
# indicating whether there is an abstract
vr_treatment <- read_csv("data-raw/isotretinoin_abstracts_supp.csv")


# Are the articles with abstract about the same sorts
# of things as those with abstracts?

# # custom stop words
# stop_words |> View()
#
# # add custom stop words to the stop words list
# stop_words <- stop_words |>
#   bind_rows(
#     tibble(word = c("virtual", "reality"),
#            lexicon = c("custom", "custom")))

# WORDS
# tokenising is key step in the analysis
# it breaks the abstracts down into words
# (or bigrams, trigrams etc)
title_word <- vr_treatment |>
  unnest_tokens(word, title) |>
  anti_join(stop_words)

# tabulate words in the two types of title
title_word_count <- title_word |>
  count(has_abstract, word, sort = TRUE) |>
  group_by(has_abstract) |>
  mutate(percent = 100*n/sum(n))

title_word_count |>
  filter(percent > 0.4) |>
  filter(has_abstract == TRUE) |>
  ggplot(aes(x = percent, y = reorder(word, percent))) +
  geom_col()

title_word_count |>
  filter(percent > 0.4) |>
  filter(has_abstract == FALSE) |>
  ggplot(aes(x = percent, y = reorder(word, percent))) +
  geom_col()

# save to figures folder
ggsave("figures/word_proportions_titles.png")

# plot percent against each other
title_word_count |>
  pivot_wider(names_from = has_abstract,
              values_from = percent,
              id_cols = word,
              names_prefix = "abstract_") |>
  ggplot(aes(x = abstract_TRUE,
             y = abstract_FALSE)) +
  geom_text(aes(label = word),
            check_overlap = TRUE,
           size = 3, colour = "seagreen") +
  geom_abline(color = "gray40", lty = 2) +
  scale_x_log10(name = "With an abstract") +
  scale_y_log10(name = "Without an abstract") +
  annotate("text", x = 0.01, y = 3,
           label = "More common in titles\nwith abstract") +
  annotate("text", x = 1.2, y = 0.05,
           label = "More common in titles\nwithout abstract") +
  theme_minimal()


# 1508 words appear in both types of title
# 5219 words do not appear in both types of title

# save to figures folder
ggsave("figures/word_proportions_abstarct_titles.png")

title_word_count_with <-
  title_word_count |>
  filter(has_abstract == TRUE)

title_word_count_without <-
  title_word_count |>
  filter(has_abstract == FALSE)

# plot wordcloud for papers wityh abstract
wordcloud(title_word_count_with$word,
          title_word_count_with$n,
          max.words = 300)

# plot wordcloud for papers without abstract
wordcloud(title_word_count_without$word,
          title_word_count_without$n,
          max.words = 300)

#################################################
####                 BIGRAMS                 ####
#################################################

# tokenising
title_bigram <- vr_treatment |>
  unnest_tokens(word, title,
                token = "ngrams", n = 2)

# tabulate words in the two types of title
title_bigram_count <- title_bigram |>
  count(has_abstract, word, sort = TRUE) |>
  group_by(has_abstract) |>
  mutate(percent = 100*n/sum(n))

title_bigram_count |>
  filter(percent > 0.3) |>
  mutate(word = reorder(word, percent))  |>
  ggplot(aes(percent, word)) +
  geom_col() +
  facet_wrap(~ has_abstract, scales = "free")


# plot proportions against each other
title_bigram_count |>
  pivot_wider(names_from = has_abstract,
              values_from = percent,
              id_cols = word,
              names_prefix = "abstract_") |>
  ggplot(aes(x = abstract_TRUE,
             y = abstract_FALSE)) +
  geom_text(aes(label = word),
            check_overlap = TRUE,
            size = 3, colour = "seagreen") +
  geom_abline(color = "gray40", lty = 2) +
  scale_x_log10(name = "With an abstract") +
  scale_y_log10(name = "Without an abstract") +
  annotate("text", x = 0.01, y = 3,
           label = "More common in titles\nwith abstract") +
  annotate("text", x = 0.7, y = 0.05,
           label = "More common in titles\nwithout abstract") +
  theme_minimal()

# 2272 bigrams appear in both types of title
# 28890  bigrams do not appear in both types of title

# save to figures folder
ggsave("figures/bigram_proportions_abstarct_titles.png")

title_bigram_count_with <-
  title_bigram_count |>
  filter(has_abstract == TRUE)

title_bigram_count_without <-
  title_bigram_count |>
  filter(has_abstract == FALSE)


wordcloud(title_bigram_count_with$word,
          title_bigram_count_with$n,
          max.words = 300)

wordcloud(title_bigram_count_without$word,
          title_bigram_count_without$n,
          max.words = 300)

# TRIGRAMS
# tokenising
title_trigram <- vr_treatment |>
  unnest_tokens(word, title,
                token = "ngrams", n = 3)

# add NA to stop words
stop_words <- stop_words |>
  bind_rows(
    tibble(word = NA_character_,
           lexicon = "custom"))

# tabulate words in the two types of title and dont include stop words
title_trigram_count <- title_trigram |>
  count(has_abstract, word, sort = TRUE) |>
  group_by(has_abstract) |>
  mutate(percent = 100*n/sum(n)) |>
  anti_join(stop_words)

title_trigram_count |> View()

title_trigram_count |>
  filter(percent > 0.3) |>
  mutate(word = reorder(word, percent))  |>
  ggplot(aes(percent, word)) +
  geom_col() +
  facet_wrap(~ has_abstract, scales = "free")

# include more trigrams in the graph
title_trigram_count |>
  filter(percent > 0.1) |>
  mutate(word = reorder(word, percent))  |>
  ggplot(aes(percent, word)) +
  geom_col() +
  facet_wrap(~ has_abstract, scales = "free")

# save to figures folder
ggsave("figures/trigram_proportions_abstarct_titles.jpg")

# plot proportions against each other
title_trigram_count |>
  pivot_wider(names_from = has_abstract,
              values_from = percent,
              id_cols = word,
              names_prefix = "abstract_") |>
  ggplot(aes(x = abstract_TRUE,
             y = abstract_FALSE)) +
  geom_text(aes(label = word),
            check_overlap = TRUE,
            size = 4, colour = "seagreen") +
  geom_abline(color = "gray40", lty = 2) +
  scale_x_log10(name = "With an abstract") +
  scale_y_log10(name = "Without an abstract") +
  annotate("text", x = 0.01, y = 3,
           label = "More common in titles\nwith abstract") +
  annotate("text", x = 0.5, y = 0.015,
           label = "More common in titles\nwithout abstract") +
  theme_minimal()


# 2272 trigrams appear in both types of title
# 28890  trigrams do not appear in both types of title

# create a bar graph showing the most common trigrams in articles with and without 
# abstracts
title_trigram_count_with <-
  title_trigram_count |>
  filter(has_abstract == TRUE)

title_trigram_count_without <- 
  title_trigram_count |>
  filter(has_abstract == FALSE)

wordcloud(title_trigram_count_with$word,
          title_trigram_count_with$n,
          max.words = 300)

# allow all of the words fit in the wordcloud

wordcloud(title_trigram_count_without$word,
          title_trigram_count_without$n,
          max.words = 300)




