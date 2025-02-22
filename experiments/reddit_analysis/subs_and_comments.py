
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
    The output should then be used in the clean_submission function.

    :param subreddit_name: The name of the subreddit
    :param limit: The number of posts to retrieve
    """
    subreddit = reddit.subreddit(subreddit_name)
    posts_and_comments = []

    for submission in subreddit.hot(limit=limit):
        cleaned_data = clean_submission(submission)
        posts_and_comments.append(cleaned_data)
    
    return "\n\n".join(posts_and_comments)

def clean_submission(submission):
    post_data = f"Title: {submission.title}\n" 
    post_data += f"Self text: {submission.selftext}\n"
    post_data += "Comments:\n"
    
    # Ensure all comments are loaded (removes the 'more comments' placeholders)
    submission.comments.replace_more(limit=0)

    # Collect the comments
    comments = []
    for comment in submission.comments.list():
        # Use regex to check for 'bot' as a whole word in a case-insensitive way.
        if comment.body and not re.search(r'\bbot\b', comment.body, re.IGNORECASE):
            comments.append(comment.body)
    
    post_data += "\n".join(comments)
    return post_data


## TO DO
# 1. Add error handling for invalid subreddit names.
# 2. Add a function to retrieve the most popular subreddits based on the number of subscribers.
# 3. Create a function to prepare text for tokenization and analysis (e.g., remove URLs, emojis, etc.).
# 4. Add a function to save the retrieved data to a file for further analysis.

### Tokenising

# Tokenization is the process of breaking text into individual words or tokens. This is a crucial step in natural 
# language processing (NLP) tasks such as sentiment analysis, text classification, and language modeling.


def tokenize_text(text):
    """
    Tokenize text by splitting it into individual words.

    :param text: The text to be tokenized
    :return: A list of tokens
    """
    tokenized = []
    for word in text.split():
        tokenized.append(word)
    return tokenized


if __name__ == '__main__':
    subreddit_name = input("Enter the subreddit name (without /r/): ")
    try:
        post_limit = int(input("How many posts would you like to retrieve? "))
    except ValueError:
        print("Invalid number entered. Defaulting to 5 posts.")
        post_limit = 5
    
    posts_and_comments = get_posts_and_comments(subreddit_name, post_limit)
    print(posts_and_comments)