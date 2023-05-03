library(httr)
library(tidyverse)
library(slackr)
library(googlesheets4)

# Google API ----
json <- Sys.getenv("TOKEN_KEY") |> 
  stringr::str_replace_all(pattern = fixed("\\n"),
                           replacement = "\n")
dec <- rawToChar( jsonlite::base64_dec( json))

gs4_auth(path = dec)

# read data ----

old_csv <- read_sheet(ss = Sys.getenv("SHEET_ID"),
                      sheet = "Form Responses 1",
                      col_types = "TccD")

current_week <- week(now(tzone = "America/Chicago"))
last_week <- current_week-1
last_week_text <- last_week

old_with_ranks <- old_csv |>
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  group_by(date) |> 
  arrange(period) |> 
  arrange(date) |> 
  mutate(rank = min_rank(period)) |> 
  ungroup() |> 
  mutate(week = week(date)) |> 
  filter(week == last_week) |> 
  select(!week) 

wins <- old_with_ranks |> 
  filter(rank == 1) |> 
  select(!date) |> 
  count(name) |> 
  arrange(desc(n))|> 
  select(name,n) |> 
  mutate(rank = min_rank(desc(n))) |> 
  mutate(emoji_rank = if_else(rank == 1,":crown:",
                              "")) |> 
  mutate(name_text = paste0(name,": ",n," ",emoji_rank,"\n")) 

wins_text <- paste0("*Week ",last_week_text," wins*",
                    "\n",
                    paste0(wins$name_text, 
                           collapse = ""))

if (week(old_with_ranks$date[[1]]) == last_week) {
  POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
       encode = "json",
       body =  list(text = wins_text,
                    type = "mrkdwn")
  )
}

