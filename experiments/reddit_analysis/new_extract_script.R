
install.packages("RedditExtractoR")
library(RedditExtractoR)
library(tidyverse)

# Find subreddits based on a search query
vacc_subreddits <- find_subreddits("isotretinoin")
write_csv(vacc_subreddits, "data-raw/isotret_subreddits.csv")

# Find URLs to threads of interest in the subreddit 'accutane'
top_urls <- find_thread_urls(subreddit = "accutane",
                             sort_by = "top",
                             period = "all",
                             keywords = "isotretinoin")

# Extract the unique thread id from the URL and combine title and text
top_urls <- top_urls %>%
  mutate(thread_id = str_extract(url, "(?<=comments/)[^/]+"),
         text2 = paste(title, text))
write_csv(top_urls, "data-raw/accutane_isotret_urls.csv")

# Define a function to retrieve comments for a single thread
get_comments_for_thread <- function(url) {
  message("Retrieving comments for: ", url)
  tryCatch({
    thread_content <- get_thread_content(url)
    
    # Extract thread details from the original post if available
    if(nrow(thread_content$threads) > 0) {
      thread_title <- thread_content$threads$title[1]
      thread_text  <- thread_content$threads$text[1]
      thread_url   <- thread_content$threads$url[1]
      thread_id    <- str_extract(thread_url, "(?<=comments/)[^/]+")
    } else {
      thread_title <- NA
      thread_text  <- NA
      thread_url   <- url
      thread_id    <- NA
    }
    
    # Retrieve and modify the comments data frame
    if(nrow(thread_content$comments) > 0) {
      thread_comments <- thread_content$comments %>%
        mutate(thread_title = thread_title,
               thread_text  = thread_text,
               thread_url   = thread_url,
               thread_id    = thread_id,
               comment_id   = as.character(comment_id))  # Coerce comment_id to character
      return(thread_comments)
    } else {
      return(tibble())
    }
  }, error = function(e) {
    message("Error retrieving URL: ", url, " - ", e)
    return(tibble())
  })
}

# Retrieve comments for each thread in top_urls and combine them into one data frame
all_comments <- map_dfr(top_urls$url, get_comments_for_thread)

# Write the combined comments data frame to a CSV file
write_csv(all_comments, "data-raw/accutane_isotret_comments_all.csv")

# Display a summary count of comments per thread
all_comments %>%
  group_by(thread_id) %>%
  summarise(n_comments = n()) %>%
  arrange(desc(n_comments)) %>%
  print()

