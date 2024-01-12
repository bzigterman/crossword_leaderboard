library(httr)
library(tidyverse)
library(rvest)
library(googlesheets4)
library(googledrive)
library(jsonlite)

# Google API ----
json <- Sys.getenv("TOKEN_KEY") |> 
  stringr::str_replace_all(pattern = fixed("\\n"),
                           replacement = "\n")
dec <- rawToChar( jsonlite::base64_dec( json))

gs4_auth(path = dec)

# Get leaderboard ----
cookie <- Sys.getenv("NYT_S") 
NYT_API_ROOT <-  "https://www.nytimes.com/svc/crosswords"
date  <-  today(tzone = "America/Chicago") #"2024-01-11" 
leaderboard_endpoint <-  paste0(NYT_API_ROOT,
                              "/v6/leaderboard/mini/",date,".json")

response <- GET(leaderboard_endpoint,
                set_cookies("NYT-S" = cookie)
                )
nyt_content <- content(response)
nyt_json <- toJSON( nyt_content$data)
nyt <- fromJSON(nyt_json, flatten = TRUE) |> 
  flatten() |> 
  janitor::clean_names() 
time <- unlist(nyt$score_seconds_spent_solving)
name <- unlist(nyt$name) |> 
  head(length(time))
nyt_new <- cbind(name,time) |> 
  as_tibble()
 
nyt_crossword_date <- date
nyt_crossword_date_text <- strftime(x = nyt_crossword_date, 
                                    tz = "US/Central",
                                    format = "%A, %B %d")

nyt_leaderboard <- nyt_new |> 
  mutate(date = date) |> 
  mutate(time = as.numeric( time)) |> 
  mutate(sec = time %% 60,
         min = floor(time / 60),
         minsec = paste0(min,":",sec)) |> 
  select(-sec, -min) |> 
  mutate(time = minsec) |> 
  select(-minsec)

# write new results ----
old_csv <- read_sheet(ss = Sys.getenv("SHEET_ID"),
                      sheet = "Form Responses 1",
                      col_types = "TccD") |> 
  mutate(time = if_else(substr(time,0,1) == ":",
                        paste0("0",time),
                        time)) |> 
  distinct(across(-Timestamp),.keep_all = TRUE)

old_csv_today <- old_csv |> 
  filter(date == nyt_crossword_date)

diffs_from_sheets <- anti_join(old_csv_today,nyt_leaderboard)
diffs_from_update <- anti_join(nyt_leaderboard,old_csv_today)

new_csv <- full_join(old_csv, diffs_from_update) |> 
  arrange(time) |> 
  arrange(date)

write_sheet(new_csv,
            ss = Sys.getenv("SHEET_ID"),
            sheet = "Form Responses 1")



