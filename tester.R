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
                      sheet = "Sheet1",
                      col_types = "ccD")

today <- force_tz(today(tzone = "America/Chicago"),tzone = "America/Chicago")

final_results <- old_csv |> 
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
  mutate(emoji_rank = case_when(
    rank == 1 ~ ":first_place_medal:",
    rank == 2 ~ ":second_place_medal:",
    rank == 3 ~ ":third_place_medal:",
    .default = ""
  )) |> 
  select(name,time,rank, emoji_rank) |> 
  mutate(nametime = paste0(name,": ",time," ",emoji_rank,"\n")) |> 
  select(nametime)
Results <- paste0("*",final_results_date_text,"*",
                  "\n",
                  paste0(nyt_leaderboard_text1$nametime, 
                         collapse = ""))
Results

# post data ----

if (final_results_date == today) {
  POST(url =  Sys.getenv("SLACK_TEST_URL"),
       encode = "json",
       body =  list(text = Results,
                    type = "mrkdwn"),
       verbose()
  )
}

