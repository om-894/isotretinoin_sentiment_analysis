
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


# Replace the placeholders below with your own Reddit API credentials.
reddit = praw.Reddit(
    client_id='-L_dO7FFuSozIciBJYoolQ',
    client_secret='oPGUzVjHqPAZHr1DD4Y-xDk_5Kse4w',
    user_agent='script:SubAndCommentRetriever:v1.0 (by /u/omquillan)'
)

###############################################################
###             Find most popular subreddits                ###
###############################################################


def get_most_popular_subreddits(limit=5):
    """
    Retrieves the most popular subreddits based on the number of subscribers.

    :param limit: The number of subreddits to retrieve
    """
    for subreddit in reddit.subreddits.popular(limit=limit):
        print(subreddit.display_name)

def clean_text(text):
    """
    Cleans the text by removing commas, newlines, dashes, and extra spaces.

    :param text: The text to clean
    :return: The cleaned text
    """
    text = text.replace('\n', ' ')
    text = text.replace('-', ' ')
    text = re.sub(r'\s+', ' ', text)  # Replace multiple spaces with a single space
    return text.strip()

def clean_submission(submission, subreddit_name):
    post_data = {
        "title": clean_text(submission.title),
        "body": clean_text(submission.selftext),
        "comments": [],
        "subreddit": subreddit_name
    }
    
    # Ensure all comments are loaded (removes the 'more comments' placeholders)
    submission.comments.replace_more(limit=0)

    # Collect the comments
    for comment in submission.comments.list():
        # Use regex to check for 'bot' as a whole word in a case-insensitive way.
        if comment.body and not re.search(r'\bbot\b', comment.body, re.IGNORECASE):
            post_data["comments"].append(clean_text(comment.body))
    
    return post_data

def get_posts_and_comments(subreddit_name, limit=10):
    """
    Retrieves posts and their comments from a given subreddit.

    :param subreddit_name: The name of the subreddit
    :param limit: The number of posts to retrieve
    """
    subreddit = reddit.subreddit(subreddit_name)
    posts_and_comments = []

    try:
        for submission in subreddit.hot(limit=limit):  # Limit the number of posts to retrieve
            print(f"Processing submission: {submission.title}")
            cleaned_data = clean_submission(submission, subreddit_name)
            posts_and_comments.append(cleaned_data)
            time.sleep(1)  # Add a delay between requests to avoid rate limits
    except Forbidden:
        print(f"Access to subreddit {subreddit_name} is forbidden. Skipping...")
        return []
    except TooManyRequests:
        print("Rate limit exceeded. Waiting for 60 seconds before retrying...")
        time.sleep(60)
        return get_posts_and_comments(subreddit_name, limit)  # Retry after delay

    return posts_and_comments

    
def write_to_csv(data, filename='reddit_posts.csv'):
    """
    Writes the data to a CSV file.

    :param data: The data to write to the CSV file
    :param filename: The name of the CSV file
    """
    with open(filename, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=["post_title", "post_body", "comments", "subreddit"], quotechar='"', quoting=csv.QUOTE_ALL)
        writer.writeheader()
        
        for post in data:
            writer.writerow({
                "post_title": post["title"],
                "post_body": post["body"],
                "comments": " ".join(post["comments"]),
                "subreddit": post["subreddit"]
            })


## TO DO
# 1. put the data to be in .csv format. I need to have a post_title column, post_body column, comment column (where all comments are combined into one paragraph)
# 2. Need to remove commas from the posts and comments. Also need to remove newlines and other spaces such as dashes, etc.
# 3. Was thinking about potentially ignoring posts that dont fully relate to acutane (or whatever subreddit i am using) but this is hard to do
#    Also this could be a good experiemtn to see if comment and post sentiment is similar or different.
# 4. Add column in csv file that has the subreddit name
# 5. Write a loop to get all of the posts from all of the subreddits that i need. 

# def check_subreddit(subreddit_name):
#     subreddit = reddit.subreddit(subreddit_name)
#     pattern = fr'\b{subreddit_name}\b'
#     return bool(re.search(pattern, subreddit.selftext, re.IGNORECASE))




if __name__ == '__main__':
    dataframe = pd.read_csv("data-raw/isotret_subreddits.csv")

    # Convert subreddit column to a list and take the first two subreddits
    retrieved_subreddits = dataframe['subreddit'].tolist()[:2]

    # Loop through the subreddits and get the posts and comments
    combined_data = []

    for subreddit in retrieved_subreddits:
        print(f"Processing subreddit: {subreddit}")
        posts_and_comments = get_posts_and_comments(subreddit)
        if posts_and_comments:
            print(f"Retrieved {len(posts_and_comments)} posts from {subreddit}")
            combined_data.extend(posts_and_comments)
        else:
            print(f"No posts retrieved from {subreddit}")

    # Write combined data to CSV
    if combined_data:
        write_to_csv(combined_data, 'combined_reddit_posts.csv')
        print(f"Data has been written to combined_reddit_posts.csv")
    else:
        print("No data to write to CSV")


# PROBLEMS:

# 1. Forbidden message
# I ran into this issue because the subreddit I was trying to connect to had gone private. So my suggestion would be to 
# first check if the subreddit you are connecting to will allow the account the bot is setup with to access that subreddit.
# potential fix: check if the subreddit is private and if it is, skip it.
# potential fix: put rate limit on posts down to 10 (ideally the 10 most popular ones)



# Nothing is going in to 