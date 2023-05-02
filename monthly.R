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

current_month <- month(now(tzone = "America/Chicago"))
last_month <- current_month-1
last_month_text <- strftime(x = ymd( paste("2023-", last_month,"-01")), 
                            tz = "US/Central",
                            format = "%B")

old_with_ranks <- old_csv |>
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  group_by(date) |> 
  arrange(period) |> 
  arrange(date) |> 
  mutate(rank = min_rank(period)) |> 
  ungroup() |> 
  mutate(month = month(date)) |> 
  filter(month == last_month) |> 
  select(!month) 

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
  
wins_text <- paste0("*",last_month_text," results*\n\n*Wins*",
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
  mutate(avg_seconds = seconds(avg)) |> 
  mutate(avg_period = seconds_to_period(avg_seconds)) |> 
  mutate(avg_text = sprintf("%d:%02d",minute(avg_period), second(avg_period))) |> 
  mutate(name_avg = paste0(name,": ",avg_text,"\n")) 

avg_times_text <- paste0("*Average times*\n",
                         paste0(avg_times$name_avg, 
                                collapse = ""))

Monthly_results <- paste0(wins_text,"\n",
               fastest_time_text,"\n",
               avg_times_text)

if (month(fastest_time$date[[1]]) == last_month) {
  POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
       encode = "json",
       body =  list(text = Monthly_results,
                    type = "mrkdwn")
  )
}

