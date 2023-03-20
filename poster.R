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
                      sheet = "Sheet1",
                      col_types = "ccD")

final_results <- old_csv |> 
  filter(date == today(tzone = "America/Chicago"))
final_results_date <- as_date( final_results$date[[1]],
                               tz = "America/Chicago")
final_results_date_text <- strftime(x = final_results_date, 
                                    tz = "US/Central",
                                    format = "%A, %B %d")

nyt_leaderboard_text1 <- final_results |> 
  select(name,time) |> 
  mutate(nametime = paste0(name,": ",time,"\n")) |> 
  select(nametime)
Results <- paste0(final_results_date_text,
                  "\n",
                  paste0(nyt_leaderboard_text1$nametime, 
                         collapse = ""))
Results

today <- today(tzone = "America/Chicago")

# post data ----

if (final_results_date == today) {
  slackr_bot(Results,
             incoming_webhook_url = Sys.getenv("SLACK_TEST_URL"))
}
