library(httr)
library(tidyverse)
library(rvest)
library(googlesheets4)

gs4_auth(
  token = Sys.getenv("GOOGLE_TOKEN"),
  email = Sys.getenv("GOOGLE_EMAIL")
)

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
nyt_crossword_date_text <- strftime(x = nyt_crossword_date, 
                                    tz = "US/Central",
                                    format = "%A, %B %d")

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
  mutate(date = as_date( nyt_crossword_date, tz = "America/Chicago")) |> 
  mutate(time = if_else(time == "Play Puzzle","--",time)) |> 
  filter(time != "--") |> 
  select(!rank)

nyt_leaderboard
nyt_leaderboard_text1 <- nyt_leaderboard |> 
  select(name,time) |> 
  mutate(nametime = paste0(name,": ",time,"\n")) |> 
  select(nametime)
Results <- paste0(nyt_crossword_date_text,
                  "\n",
                  paste0(nyt_leaderboard_text1$nametime, 
                         collapse = ""))
Results

old_csv <- read_sheet(Sys.getenv("SHEET_ID"),
                      col_types = "ccD")
  
old_csv_today <- old_csv |> 
  filter(date == nyt_crossword_date)

new_csv <- old_csv |> 
  filter(date != nyt_crossword_date) |> 
  full_join(nyt_leaderboard)

write_sheet(new_csv,
            ss = Sys.getenv("SHEET_ID"),
            sheet = "Sheet1")


