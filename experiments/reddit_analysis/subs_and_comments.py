
#!/usr/bin/env python3
import praw

# This script retrieves posts and their comments from a specified subreddit.
# Please ensure you have installed PRAW (run: pip install praw)
# and have your Reddit API credentials from your Reddit developer account.

# To get API access:

# 1. Go to https://www.reddit.com/prefs/apps
# 2. Click on "Create App"
# 3. Choose "script" as the app type
# 4. Fill in the details (name = SubAndCommentRetriever, description = "Script to retrieve posts and comments from a subreddit", about url = http://localhost, redirect uri = http://localhost)

# Replace the placeholders below with your own Reddit API credentials.
reddit = praw.Reddit(
    client_id='YOUR_CLIENT_ID',
    client_secret='YOUR_CLIENT_SECRET',
    user_agent='script:reddit_post_comment_retriever:v1.0 (by /u/YOUR_USERNAME)'
)

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
        
        # Iterate over a flat list of all comments in the submission
        for comment in submission.comments.list():
            # Some comments might be deleted or removed
            if comment.body:
                print("  - ", comment.body)
        print("=" * 80)

if __name__ == '__main__':
    subreddit_name = input("Enter the subreddit name (without /r/): ")
    try:
        post_limit = int(input("How many posts would you like to retrieve? "))
    except ValueError:
        print("Invalid number entered. Defaulting to 5 posts.")
        post_limit = 5
    get_posts_and_comments(subreddit_name, post_limit)
