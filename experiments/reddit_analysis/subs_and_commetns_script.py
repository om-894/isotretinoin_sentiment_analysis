#!/usr/bin/env python3
import praw
import re
import csv
import pandas as pd
from prawcore.exceptions import Forbidden, TooManyRequests
import time

# This script retrieves posts and their comments from a specified subreddit.
# Please ensure you have installed PRAW (run: pip install praw)
# and have your Reddit API credentials from your Reddit developer account.

# To get API access:

# 1. Go to https://www.reddit.com/prefs/apps
# 2. Click on "Create App"
# 3. Choose "script" as the app type
# 4. Fill in the details (name = SubAndCommentRetriever, description = "Script to retrieve posts and comments from a subreddit", about url = http://localhost, redirect uri = http://localhost)

# client ID: -L_dO7FFuSozIciBJYoolQ
# secret: oPGUzVjHqPAZHr1DD4Y-xDk_5Kse4w


def clean_text(text):
    """
    Cleans the text by removing commas, newlines, dashes and extra spaces.
    """
    text = text.replace(',', ' ').replace('\n', ' ').replace('-', ' ')
    text = re.sub(r'\s+', ' ', text)  # Replace multiple spaces with a single space
    return text.strip()

def clean_submission(submission, subreddit_name):
    """
    Cleans post title, body, and comments and adds subreddit name.
    """
    post_data = {
        "subreddit": subreddit_name,
        "id": submission.id,
        "title": clean_text(submission.title),
        "body": clean_text(submission.selftext),
        "comments": []
    }
    try:
        submission.comments.replace_more(limit=0)
        for comment in submission.comments.list():
            if comment.body and not re.search(r'\bbot\b', comment.body, re.IGNORECASE):
                post_data["comments"].append(clean_text(comment.body))
    except Exception as e:
        print(f"Error retrieving comments: {e}")
    return post_data

def get_posts_and_comments(reddit, subreddit_name, limit=5):
    """
    Retrieves posts and their comments from a given subreddit.
    """
    try:
        subreddit = reddit.subreddit(subreddit_name)
        posts_and_comments = []
        for submission in subreddit.hot(limit=limit):
            posts_and_comments.append(clean_submission(submission, subreddit_name))
        return posts_and_comments
    except Forbidden:
        print(f"Access denied to r/{subreddit_name}. Skipping...")
    except TooManyRequests as e:
        wait_time = int(e.response.headers.get('Retry-After', 60))
        print(f"Rate limit reached. Waiting for {wait_time} seconds...")
        time.sleep(wait_time)
        return get_posts_and_comments(reddit, subreddit_name, limit)
    except Exception as e:
        print(f"Unexpected error with r/{subreddit_name}: {e}")
    return []

def get_top_posts_and_comments(reddit, subreddit_name, limit=10):
    """
    Retrieves top posts and their comments from a given subreddit.
    """
    try:
        subreddit = reddit.subreddit(subreddit_name)
        posts_and_comments = []
        for submission in subreddit.top(time_filter="all", limit=limit):        # default is "all"
            posts_and_comments.append(clean_submission(submission, subreddit_name))
        return posts_and_comments
    except Forbidden:
        print(f"Access denied to r/{subreddit_name}. Skipping...")
    except TooManyRequests as e:
        wait_time = int(e.response.headers.get('Retry-After', 60))
        print(f"Rate limit reached. Waiting for {wait_time} seconds...")
        time.sleep(wait_time)
        return get_top_posts_and_comments(reddit, subreddit_name, limit)
    except Exception as e:
        print(f"Unexpected error with r/{subreddit_name}: {e}")
    return []

def write_to_csv(data, filename):
    """
    Writes the data to a CSV file, with each comment on a separate row.
    """
    filepath = f"data-raw/reddit-posts-and-comments/{filename}"
    with open(filepath, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(["subreddit", "post_id", "post_title", "post_body", "comment"])
        for post in data:
            if post["comments"]:
                for comment in post["comments"]:
                    writer.writerow([post["subreddit"], post["id"], post["title"], post["body"], comment])
            else:
                writer.writerow([post["subreddit"], post["id"], post["title"], post["body"], "No comments"])

if __name__ == '__main__':
    reddit = praw.Reddit(
        client_id='-L_dO7FFuSozIciBJYoolQ',
        client_secret='oPGUzVjHqPAZHr1DD4Y-xDk_5Kse4w',
        user_agent='script:SubAndCommentRetriever:v1.0 (by /u/omquillan)'
    )

    try:
        dataframe = pd.read_csv("data-raw/isotret_subreddits.csv")
        subreddit_list = dataframe['subreddit'].dropna().tolist()  # Process as many subreddits as possible
        all_data = []
        
        for subreddit_name in subreddit_list:
            print(f"Fetching data from r/{subreddit_name}...")
            posts_and_comments = get_top_posts_and_comments(reddit, subreddit_name, limit=10)
            # posts_and_comments = get_top_posts_and_comments(reddit, subreddit_name, limit=10)
            if posts_and_comments:
                # write_to_csv(posts_and_comments, f'{subreddit_name}_reddit_posts.csv')
                # print(f"Saved {subreddit_name}_reddit_posts.csv")
                all_data.extend(posts_and_comments)  # Store all data for combined CSV
            else:
                print(f"No data retrieved for r/{subreddit_name}")
        
        # Write combined file with all subreddits
        if all_data:
            write_to_csv(all_data, 'all_subreddits_reddit_posts_possible.csv')
            print("Saved all_subreddits_reddit_posts.csv")
    except Exception as e:
        print(f"Error reading subreddit list: {e}")


# first post in csv for AccutaneDamage subreddit should be this:
# https://www.reddit.com/r/AccutaneDamage/comments/fwmkhc/response_to_did_accutane_cause_my_health_issues/


# To do:
# 1. combine all subreddits into one file (but do this additionally to having them separate 
# - so i can compare sentiments between subreddits)
# 2. add column for subreddit name
# 3. new data folder for each individual subreddit
# 4. find and filter for most popular posts for each subreddit (n=10)
#    - Forget this, though script has been altered to do this which is good. 
#    - It would be better to filter for posts relevant to isotretinoin or accutane
# 5. test the limits on how many posts can be retrieved
# 6. Sentiment analysis on each comment?

### Unused functions ###

# def check_subreddit(subreddit_name):
#     subreddit = reddit.subreddit(subreddit_name)
#     pattern = fr'\b{subreddit_name}\b'
#     return bool(re.search(pattern, subreddit.selftext, re.IGNORECASE))