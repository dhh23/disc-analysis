from src.common_basis import *

i = load_incel_parquet()
df = i.incel_posts
df2 = df.drop(columns='post_html')


def sample_threads_from_df(dataframe, sample_size: int, query: str):
    subsample = df.sample(sample_size)
    contains = subsample[subsample['post_content'].str.contains(query)]
    # count threads with most mentions of query
    thread_counts = contains.pivot_table(index=["thread_id"], aggfunc='size').sort_values(ascending=False)

    # retrieve posts from threads with top 10 most mentions of query
    thread_ids = thread_counts[:10].index
    queried_posts = df2[df2['thread_id'].isin(thread_ids)]
    queried_threads = i.incel_threads[i.incel_threads["thread_id"].isin(thread_ids)]
    queried_df = queried_threads.merge(queried_posts, how="inner", on="thread_id")

    return queried_df, queried_threads, queried_posts,


fakecel_df, fakecel_threads, fakecel_posts = sample_threads_from_df(df2, 10000, "fakecel")
truecel_df, truecel_threads, truecel_posts = sample_threads_from_df(df2, 10000, "truecel")

