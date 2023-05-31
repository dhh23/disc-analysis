from bs4 import BeautifulSoup
from src.common_basis import *

# load incel data
i = load_incel_parquet()
df = i.incel_posts

# new df with only post html and post id
df_for_processing = df[["post_html", "post_id"]]

# extract clean posts from html
df_for_processing['post_text'] = df_for_processing['post_html']\
    .apply(lambda x: BeautifulSoup(x, 'html.parser')
    .find('div', class_='bbWrapper')
    .find_all(string=True, recursive=False))\
    .apply(lambda x: ' '.join(x))

# drop html column from df to avoid pyarrow bug
df2 = df.drop(columns='post_html')

# merge cleaned data with original data
df_cleaned = df_for_processing.merge(df2, on="post_id")
df_cleaned.drop(columns=["post_html", "contains_fakecel", "contains_truecel"])


# subsets of posts containing fakecel/truecel
fakecel_df = df_cleaned[df_cleaned["cleaned_text"].str.contains("fakecel")]
truecel_df = df_cleaned[df_cleaned["cleaned_text"].str.contains("truecel")]
