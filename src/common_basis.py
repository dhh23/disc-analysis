from dataclasses import dataclass
from typing import Literal

import keyring
import getpass
import sqlalchemy
import yaml
import pandas as pd
from hereutil import here
from sqlalchemy import text
from sqlalchemy.exc import SQLAlchemyError
from sqlalchemy.future import Engine, Connection


def get_connection() -> (Engine, Connection):
    eng = None
    con = None
    password = None
    while con is None:
        with here("db_params.yaml").open('r') as f:
            db_params = yaml.safe_load(f)
        if here("db_secret.yaml", warn=False).exists():
            password = yaml.safe_load(here("db_secret.yaml").open('r'))['db_pass']
        else:
            try:
                password = keyring.get_password(db_params['db_name'], "DB_PASS")
            except keyring.errors.NoKeyringError:
                pass
        if password is None:
            password = ""
        try:
            eng = sqlalchemy.create_engine(
                "mariadb+pymysql://" + db_params['db_user'] + ":" + password + "@" + db_params['db_host'] + "/" +
                db_params['db_name'] + "?charset=utf8mb4&autocommit&local_infile",
                isolation_level="AUTOCOMMIT"
            )
            con = eng.connect()
        except SQLAlchemyError as err:
            eng = None
            con = None
            password = getpass.getpass(f"Database password (connection attempt failed with {err}): ")
            if password == "":
              raise ValueError("Empty password given")
            try:
                if keyring.get_password(db_params['db_name'], "DB_PASS") is not None:
                    keyring.delete_password(db_params['db_name'], "DB_PASS")
                keyring.set_password(db_params['db_name'], "DB_PASS", password)
            except keyring.errors.NoKeyringError:
                pass
    return eng, con


def set_session_storage_engine(con: Connection, engine: str):
    con.execute(text("SET SESSION storage_engine="+engine))


@dataclass
class AbortionData:
    abortion_conversations: pd.DataFrame
    abortion_tweets: pd.DataFrame
    abortion_tweet_hashtags: pd.DataFrame
    abortion_tweet_urls: pd.DataFrame
    abortion_tweet_mentions: pd.DataFrame
    abortion_matching_tweet_ids: pd.DataFrame

    def __repr__(self):
        return "AbortionData(abortion_conversations, abortion_tweets, abortion_tweet_hashtags, abortion_tweet_urls, " \
               "abortion_tweet_mentions, abortion_matching_tweet_ids)"


def load_abortion_parquet(dtype_backend: Literal['pyarrow', 'numpy_nullable'] = 'pyarrow') -> AbortionData:
    return AbortionData(
        pd.read_parquet(here("data/input/parquet/abortion_conversations.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/abortion_tweets.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/abortion_tweet_hashtags.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/abortion_tweet_urls.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/abortion_tweet_mentions.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/abortion_matching_tweet_ids.parquet"), dtype_backend=dtype_backend)
    )


@dataclass
class LynchingData:
    lynching_conversations: pd.DataFrame
    lynching_tweets: pd.DataFrame
    lynching_tweet_hashtags: pd.DataFrame
    lynching_tweet_urls: pd.DataFrame
    lynching_tweet_mentions: pd.DataFrame
    lynching_matching_tweet_ids: pd.DataFrame

    def __repr__(self):
        return "LynchingData(lynching_conversations, lynching_tweets, lynching_tweet_hashtags, lynching_tweet_urls, " \
               "lynching_tweet_mentions, lynching_matching_tweet_ids)"


def load_lynching_parquet(dtype_backend: Literal['pyarrow', 'numpy_nullable'] = 'pyarrow') -> LynchingData:
    return LynchingData(
        pd.read_parquet(here("data/input/parquet/lynching_conversations.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/lynching_tweets.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/lynching_tweet_hashtags.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/lynching_tweet_urls.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/lynching_tweet_mentions.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/lynching_matching_tweet_ids.parquet"), dtype_backend=dtype_backend)
    )


@dataclass
class IncelData:
    incel_threads: pd.DataFrame
    incel_posts: pd.DataFrame
    incel_users: pd.DataFrame
    incel_quotes: pd.DataFrame

    def __repr__(self):
        return "IncelData(incel_threads, incel_posts, incel_users, incel_quotes)"


def load_incel_parquet(dtype_backend: Literal['pyarrow', 'numpy_nullable'] = 'pyarrow') -> IncelData:
    return IncelData(
        pd.read_parquet(here("data/input/parquet/incel_threads.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/incel_posts.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/incel_users.parquet"), dtype_backend=dtype_backend),
        pd.read_parquet(here("data/input/parquet/incel_quotes.parquet"), dtype_backend=dtype_backend),
    )


__all__ = ["get_connection", "set_session_storage_engine", "load_abortion_parquet", "load_lynching_parquet", "load_incel_parquet"]
