

# This script does not work. It was intended to retrieve reddit but i used python
# instead. I am leaving it here for reference. It is not used in the analysis.



### load necessary packages ----------------------------------------------------

library(RedditExtractoR)
library(tidyverse)

# find subreddits based on a search query
iso_subreddits <- find_subreddits("isotretinoin")

# 24-11-2024 gave 33 subreddits
write_csv(iso_subreddits, "data-raw/isotret_subreddits.csv")

# find URLs to threads of interest in subrettit named 'accutane'.
top_urls <- find_thread_urls(subreddit = "accutane",
                             sort_by = "top",
                             period = "all",
                             keywords = "isotretinoin")

# 24-11-2024 gave 233 url


# get the unique thread id from the url and
# add it as a column to the data frame
top_urls <- top_urls |>
  mutate(thread_id = str_extract(url, "(?<=comments/)[^/]+"))

# combine the title a and text of the original post in the thread

top_urls <- top_urls  |>
  mutate(text2 = paste(title, text))
write_csv(top_urls, "data-raw/accutane_isotret_urls.csv")
# to anaylze that text in R you can use the text2 column from
# this file


# to analyse the text in taguette you may want to have the
# text/title for each original post in a text file
# this will write the each value of the text2 column to a file
# named with the corresponding thread_id
# walk2(top_urls$text2,
#       top_urls$thread_id,
#       ~ writeLines(.x, paste0(.y, ".txt")))


#### Getting all the comments from a thread ####

# use URLs to threads of interest to retrieve comments out of these threads
# I've done only to the first four threads for speed.
# NB Might be a good idea to do this in batches for the whole dataset.
# See later

threads_contents <- get_thread_content(top_urls$url[1:4])
# threads_contents is a list with 2 elements
# 1. threads_contents[["threads"]] - a data frame with with info
#     about the original post
# 2. threads_contents[["comments"]] - a data frame the comment

# write the data frame comments to  file
write_csv(threads_contents$comments, "data-raw/accutane_isotret_comments.csv")

threads_contents$comments |>
  group_by(url) |>
  count()
# url                                                                                          n
# <chr>                                                                                      <int>
# 1 https://www.reddit.com/r/Accutane/comments/10qid7c/last_dose_tomorrow_any_advice_for_…     4
# 2 https://www.reddit.com/r/Accutane/comments/1cgpewr/starting_my_journey_today_im_a_bit…    13
# 3 https://www.reddit.com/r/Accutane/comments/p6oe3s/finally_back_on_accutane_next_week_…    28
# 4 https://www.reddit.com/r/Accutane/comments/z2athc/starting_isotretinoin_20mg_and_pred…    14

threads_contents$threads |>
  select(url, comments)

# why aren't these the same.

# I have checked the actual pages. The number of comments says 20 directly
# after the original post. But the actual number of comments is 13.
# The same is true for the other threads. The number of comments is less than
# printed number. Perhaps that total includes deleted comments or edits.
# Any way upshot is that the number of comments in the data frame is correct.


#### combining dataframes ####





