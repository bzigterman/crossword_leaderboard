library(httr)
library(tidyverse)
library(rvest)

url <- "https://www.nytimes.com/puzzles/leaderboards/"
cookie <- Sys.getenv("NYT_S")
nyt <- GET(url,
           set_cookies("NYT-S" = cookie)
)
nyt_content <- content(nyt)
nyt_table_ranks <- read_html(nyt) |> 
  html_elements(".lbd-board__items") |> 
  html_elements(".lbd-score__rank") |> 
  html_text2() |> 
  as_tibble() |> 
  mutate(rank = value) |> 
  select(rank)

nyt_table_names <- read_html(nyt) |> 
  html_elements(".lbd-board__items") |> 
  html_elements(".lbd-score__name") |> 
  html_text2() |> 
  as_tibble() |> 
  mutate(name = if_else(value == "Ben (you)","Ben",value)) |> 
  select(name)

nyt_table_times <- read_html(nyt) |> 
  html_elements(".lbd-board__items") |> 
  html_elements(".lbd-score__time") |> 
  html_text2() |> 
  as_tibble() |> 
  mutate(time = value) |> 
  select(time)

nyt_leaderboard <- cbind(nyt_table_names,nyt_table_ranks) |> 
  cbind(nyt_table_times) |> 
  mutate(date = today(tzone = "America/Chicago"))
nyt_leaderboard

write_csv(x = nyt_leaderboard,
          file = "leaderboard.csv",
          append = TRUE)

