
#!/usr/bin/env python3
import praw
import re

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


def get_posts_and_comments(subreddit_name, limit=5):
    """
    Retrieves posts and their comments from a given subreddit.

    :param subreddit_name: The name of the subreddit
    :param limit: The number of posts to retrieve
    """
    subreddit = reddit.subreddit(subreddit_name)
    
    for submission in subreddit.hot(limit=limit):
        print("Title: ", submission.title)
        print("Score: ", submission.score)
        print("URL: ", submission.url)
        print("Self text: ", submission.selftext)
        print("Comments:")
        
        # Ensure all comments are loaded (removes the 'more comments' placeholders)
        submission.comments.replace_more(limit=0)

        # Print the comments
        for comment in submission.comments.list():
            # Use regex to check for 'bot' as a whole word in a case-insensitive way.
            if comment.body and re.search(r'\bbot\b', comment.body, re.IGNORECASE):
                continue
            print("  -", comment.body)
        print("=" * 80)

if __name__ == '__main__':
    subreddit_name = input("Enter the subreddit name (without /r/): ")
    try:
        post_limit = int(input("How many posts would you like to retrieve? "))
    except ValueError:
        print("Invalid number entered. Defaulting to 5 posts.")
        post_limit = 5
    get_posts_and_comments(subreddit_name, post_limit)


## TO DO
# 1. Add error handling for invalid subreddit names.
# 2. Add a function to retrieve the most popular subreddits based on the number of subscribers.
# 3. Create a function to prepare text for tokenization and analysis (e.g., remove URLs, emojis, etc.).
# 4. Add a function to save the retrieved data to a file for further analysis.