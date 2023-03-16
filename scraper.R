library(httr)
library(tidyverse)
library(rvest)
library(slackr)

# webhook_url <- Sys.getenv("SLACK_TEST_URL")

# create_config_file(
#   filename = "~/.slackr",
#   token = Sys.getenv("SLACK_TOKEN"),
#   incoming_webhook_url = Sys.getenv("SLACK_TEST_URL"),
#   channel = "#test"
# )
# 
# slackr_setup()

url <- "https://www.nytimes.com/puzzles/leaderboards/"
cookie <- Sys.getenv("NYT_S")
nyt <- GET(url,
           set_cookies("NYT-S" = cookie)
)
nyt_content <- content(nyt)

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

slackr_bot(Results,
           incoming_webhook_url = Sys.getenv("SLACK_TEST_URL"))

# slackr_bot('Test message', 
#            incoming_webhook_url = Sys.getenv("SLACK_TEST_URL"))

# old_csv <- read_csv("leaderboard.csv")
# 
# if (nrow(old_csv) < nrow(nyt_leaderboard)) {
#   write_csv(x = nyt_leaderboard,
#             file = "leaderboard.csv",
#             append = TRUE)
# }
