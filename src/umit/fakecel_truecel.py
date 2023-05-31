from src.common_basis import *
from bs4 import BeautifulSoup
import pandas as pd


i = load_incel_parquet()

# clean up the quoted text from the dataframe
i.incel_posts['post_text'] = i.incel_posts['post_html']\
    .apply(lambda x: BeautifulSoup(x, 'html.parser')
    .find('div', class_='bbWrapper')
    .find_all(string=True, recursive=False))\
    .apply(lambda x: ' '.join(x))

df = i.incel_posts
df2 = df.drop(columns='post_html')

def sample_threads_from_df(df, sample_size: int, query: str):
    subsample = df.sample(sample_size)
    contains = subsample[subsample['cleaned_text'].str.contains(query)]
    # count threads with most mentions of query
    thread_counts = contains.pivot_table(index=["thread_id"], aggfunc='size').sort_values(ascending=False)

    # retrieve posts from threads with top 10 most mentions of query

    thread_ids = thread_counts[:20].index
    queried_posts = df2[df2['thread_id'].isin(thread_ids)]
    queried_threads = i.incel_threads[i.incel_threads["thread_id"].isin(thread_ids)]
    queried_df = queried_threads.merge(queried_posts, how="inner", on="thread_id")

    return queried_df


# fakecel_df = sample_threads_from_df(df2, 2226024, "fakecel")
# truecel_df = sample_threads_from_df(df2, 2226024, "truecel")


fakecel_posts = df2[df2["cleaned_text"].str.contains("fakecel")]
truecel_posts = df2[df2["cleaned_text"].str.contains("truecel")]


fakecel_posts['Query count'] = fakecel_posts['cleaned_text'].str.count("fakecel").astype(int)
truecel_posts['Query count'] = truecel_posts['cleaned_text'].str.count("truecel").astype(int)

counts_true = truecel_posts.groupby('thread_id')['Query count'].sum()
counts_fake = fakecel_posts.groupby('thread_id')['Query count'].sum()


#post_content = BeautifulSoup(post["post_html"], 'html.parser').find("div", class_="bbWrapper").find_all(text=True, recursive=False)

#%%


# greycel / postmaxxer tables

greycels = i.incel_users.loc[i.incel_users["user_title"] == "Greycel"][i.incel_users["user_total_posts"]<100]

postmaxxers = i.incel_users.loc[i.incel_users["user_title"].isin(["Overlord", "Major", "Mythic", "Admiral", "Legend", "Luminary", "Commander", "Paragon", "Demigod"])]

# users 387 and 2119 are anomalies in the postmaxxer subset, they have joined in 2022 and they have over 20k posts,
# whereas most users joined in 2022 hace around 2-3k
# could their discussions have something that defines them?


# Merge user IDs with posts DataFrame
merged_df = pd.merge(posts, user_ids, on='user_id', how='left')

# Count the number of posts for each user
post_counts = merged_df['user_id'].value_counts().reset_index()
post_counts.columns = ['user_id', 'post_count']

# Merge with user IDs table
result = pd.merge(user_ids, post_counts, on='user_id', how='left').fillna(0)


post_counts = df2["poster_id"].value_counts().reset_index()
new_poster_stats = pd.merge(post_counts, i.incel_users, left_on="poster_id", right_on="user_id", how="left")


top_posters_lounge = df2.groupby("poster_id")["post_content"].size()

i.incel_threads[(i.incel_threads.posts < 160) & (i.incel_threads.thread_label == 'RageFuel')].sort_values('posts',ascending=False).head(20)

thread_labels = i.incel_threads["thread_label"].unique()