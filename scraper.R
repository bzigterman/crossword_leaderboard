library(httr)
library(tidyverse)
library(rvest)
library(slackr)

url <- "https://www.nytimes.com/puzzles/leaderboards/"
cookie <- Sys.getenv("NYT_S")
nyt <- GET(url,
           set_cookies("NYT-S" = cookie)
)
nyt_content <- content(nyt)

nyt_crossword_date <- read_html(nyt) |> 
  html_element(".lbd-type__date") |> 
  html_text2() |> 
  mdy(tz = "America/Chicago")

today <- today(tzone = "America/Chicago")

nyt_leaderboard <- read_html(nyt) |> 
  html_elements(".lbd-board__items") |> 
  html_elements(".lbd-score") |> 
  html_text2() |> 
  as_tibble() |> 
  separate_wider_delim(cols = value, delim = "\n", names = c("rank",
                                                             "blank",
                                                             "name",
                                                             "blank2","time")) |> 
  select(rank,name,time) |> 
  mutate(name = if_else(name == "Ben (you)","Ben",name)) |> 
  mutate(date = today(tzone = "America/Chicago")) |> 
  mutate(time = if_else(time == "Play Puzzle","--",time))

nyt_leaderboard
nyt_leaderboard_text1 <- nyt_leaderboard |> 
  select(name,time) |> 
  mutate(nametime = paste0(name,": ",time,"\n")) |> 
  select(nametime)
Results <- paste0(nyt_leaderboard_text1$nametime, collapse = "")  

if (nyt_crossword_date == today) {
  slackr_bot(Results,
             incoming_webhook_url = Sys.getenv("SLACK_TEST_URL"))
}

