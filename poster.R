library(httr)
library(tidyverse)
library(slackr)
library(googlesheets4)
library(gargle)
library(sodium)
library(googledrive)

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


old_adding_streaks <- old_csv |> 
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  group_by(date) |> 
  arrange(period) |> 
  arrange(date) |> 
  mutate(rank = min_rank(period)) |> 
  ungroup() |> 
  filter(rank == 1) |> 
  select(name, date, rank) |> 
  mutate(lagged = lag(name))  |> 
  mutate(start = if_else(lagged == name,FALSE,TRUE)) 

old_adding_streaks[1,"start"] <- TRUE

streaks <- old_adding_streaks|> 
  mutate(start_id = if_else(start,1,0)) |> 
  mutate(streak_id = cumsum(start_id)) |> 
  group_by(streak_id) |> 
  mutate(streak = row_number()) |> 
  ungroup() |> 
  select(name, date, streak)

old_csv_with_streaks <- full_join(old_csv, streaks)

today <- force_tz(today(tzone = "America/Chicago"),tzone = "America/Chicago")

final_results <- old_csv_with_streaks |> 
  filter(date == today)
final_results_date <- force_tz(as_date( final_results$date[[1]],
                                        tz = "America/Chicago"),
                               tzone = "America/Chicago")
final_results_date_text <- strftime(x = final_results_date, 
                                    tz = "US/Central",
                                    format = "%A, %B %d")

nyt_leaderboard_text1 <- final_results |> 
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  mutate(rank = min_rank(period)) |> 
  arrange(period) |> 
  mutate(emoji_rank = case_when(
    rank == 1 ~ ":first_place_medal:",
    rank == 2 ~ ":second_place_medal:",
    rank == 3 ~ ":third_place_medal:",
    .default = ""
  )) |> 
  mutate(streak_text = if_else(rank == 1,
                               if_else(streak > 3,
                                       paste0("(",streak,"-day streak)"),
                                       ""
                               )
                               ,
                               "")) |> 
  select(name,time,rank, emoji_rank, streak_text) |> 
  mutate(nametime = paste0(name,": ",time," ",emoji_rank," ",streak_text,"\n")) |> 
  select(nametime)
Results <- paste0("*",final_results_date_text,"*",
                  "\n",
                  paste0(nyt_leaderboard_text1$nametime, 
                         collapse = ""))

# post data ----
if (final_results_date == today) {
  POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
       encode = "json",
       body =  list(text = Results,
                    type = "mrkdwn")
  )
}
