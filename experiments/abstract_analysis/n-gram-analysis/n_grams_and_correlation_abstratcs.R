
# An n-gram is a sequence of n adjacent symbols in particular order.

# This script demonstrates how to analyze relationships between words using n-grams and correlations.
# We will explore tokenizing text into bigrams, filtering and counting n-grams, analyzing bigrams,
# using bigrams for sentiment analysis, visualizing networks of bigrams, and examining word correlations.

# Load necessary libraries -----------------------------------------------------
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)
library(tidyverse)