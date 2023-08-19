library(httr)
library(tidyverse)
library(rvest)
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

# Get leaderboard ----

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

names <- read_html(nyt) |> 
  html_elements(".lbd-board__items") |> 
  html_elements(".lbd-score__name") |> 
  html_text2() |> 
  as_tibble() |> 
  mutate(name = value) |> 
  select(!value)

times <- read_html(nyt) |> 
  html_elements(".lbd-board__items") |> 
  html_elements(".lbd-score__time") |> 
  html_text2() |> 
  as_tibble() |> 
  mutate(time = value) |> 
  select(!value)

if (nrow(times) < nrow(names)) {
  times <- times |>  
    add_row()
}

nyt_leaderboard <- cbind(names,times) |> 
  mutate(name = if_else(name == "Ben (you)","Ben",name)) |> 
  mutate(date = as_date( nyt_crossword_date, tz = "America/Chicago")) |> 
  mutate(time = if_else( is.na(time),"--",time)) |> 
  filter(time != "--")

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



