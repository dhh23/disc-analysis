from src.common_basis import *
import pyarrow as pa
import pyarrow.compute as pc
import pandas as pd
import datetime

import datetime


#%%

i = load_incel_parquet()
df = i.incel_posts
df2 = df.drop(columns='post_html')

subsample = df2.sample(10000)
contains_fakecel = subsample[subsample['post_content'].str.contains('fakecel')]


# count threads with most mentions of "fakecel"
thread_counts = contains_fakecel.pivot_table(index=["thread_id"], aggfunc ='size').sort_values(ascending=False)
print(thread_counts)

# retrieve posts from threads with most mentions of "fakecel"
thread_ids = thread_counts[:10].index

fakecel_threads = df2[df2['thread_id'].isin(thread_ids)]

fakecel_threads