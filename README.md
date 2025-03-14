# stage_3_project

# Reddit Data Extraction and Analysis  

## Overview  
This repository contains scripts for extracting and analyzing Reddit data, specifically related to isotretinoin (Accutane). The scripts use both Python and R to retrieve posts, comments, and subreddit information for further analysis.  

## Scripts  

### 1. `subs_and_commetns_script.py`  
A Python script that retrieves Reddit posts and comments from specified subreddits using the PRAW library.  

- Fetches posts and comments from a list of subreddits.  
- Cleans and processes text data.  
- Filters subreddits related to isotretinoin/Accutane.  
- Saves data as CSV files.  
- Implements error handling for API rate limits and access restrictions.  

**Requirements:**  
- `praw` (install with `pip install praw`)  
- Reddit API credentials (client ID, secret, user agent)  

---

### 2. `new_extract_script.R`  
An R script using the `RedditExtractoR` package to extract subreddit and thread information.  

- Searches for subreddits related to isotretinoin.  
- Retrieves top threads from r/Accutane containing relevant keywords.  
- Extracts thread details (title, text, comments).  
- Saves extracted data to CSV files.  

**Requirements:**  
- `RedditExtractoR`, `tidyverse` (install with `install.packages("RedditExtractoR", "tidyverse")`)  

---

### 3. `retrieve-reddit.R`  
An R script that automates subreddit and comment retrieval.  

- Searches for subreddits containing "isotretinoin."  
- Extracts thread URLs from r/Accutane.  
- Retrieves posts and comments from multiple threads.  
- Saves structured data to CSV for further analysis.  

**Requirements:**  
- `RedditExtractoR`, `tidyverse`  

---

## How to Use  

### **Python (`subs_and_commetns_script.py`)**  
1. Ensure you have a Reddit API account and API credentials.  
2. Install dependencies:  
   ```bash
   pip install praw pandas
