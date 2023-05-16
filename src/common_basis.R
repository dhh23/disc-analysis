library(here)
library(tidyverse)
library(hscidbutil)
library(gghsci)
library(googlesheets4)

con <- get_connection()

register_tables(con, "dhh23")


