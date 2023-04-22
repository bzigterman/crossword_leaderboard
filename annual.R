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

current_year <- year(now(tzone = "America/Chicago"))
last_year <- current_year-1
last_year_text <- last_year

old_with_ranks <- old_csv |>
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  group_by(date) |> 
  arrange(period) |> 
  arrange(date) |> 
  mutate(rank = min_rank(period)) |> 
  ungroup() |> 
  mutate(year = year(date)) |> 
  filter(year == last_year) |> 
  select(!year) 

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

wins_text <- paste0("*",last_year_text," results*\n\n*Wins*",
                    "\n",
                    paste0(wins$name_text, 
                           collapse = ""))

fastest_time <- old_with_ranks |> 
  arrange(period) |> 
  head(n = 3) |> 
  mutate(date_text = strftime(x = date, 
                              tz = "US/Central",
                              format = "%a, %b %d")) |> 
  mutate(timenamedate = paste0(time," by ",name," on ",date_text,"\n"))

fastest_time_text <- paste0("*Fastest times*\n",
                            paste0(fastest_time$timenamedate, 
                                   collapse = ""))


avg_times <- old_with_ranks |> 
  group_by(name) |> 
  mutate(avg = round(mean(seconds))) |> 
  ungroup() |> 
  select(name, avg) |> 
  distinct() |> 
  arrange(avg) |> 
  mutate(avg_minutes = floor(avg/60)) |> 
  mutate(avg_text = paste0(avg_minutes,":",avg-(avg_minutes*60)) ) |> 
  mutate(name_avg = paste0(name,": ",avg_text,"\n")) 

avg_times_text <- paste0("*Average times*\n",
                         paste0(avg_times$name_avg, 
                                collapse = ""))

Annual_results <- paste0(wins_text,"\n",
                         fastest_time_text,"\n",
                         avg_times_text)

if (year(fastest_time$date[[1]]) == last_year) {
  POST(url =  Sys.getenv("SLACK_TEST_URL"),
       encode = "json",
       body =  list(text = Annual_results,
                    type = "mrkdwn")
  )
}

