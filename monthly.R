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
  mutate(name_text = paste0(name,": ",n,"\n")) 

wins_text <- paste0("*",last_month_text," results*\n\n*Wins*",
                  "\n",
                  paste0(wins$name_text, 
                         collapse = ""))
wins_text

fastest_time <- old_with_ranks |> 
  arrange(period) |> 
  head(n = 1)

fastest_time_date <- strftime(x = fastest_time$date, 
                              tz = "US/Central",
                              format = "%A, %B %d")

fastest_time_text <- paste0("*Fastest time*:\n",fastest_time$time,", by ",
                            fastest_time$name," on ",
                            fastest_time_date)
fastest_time_text


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
avg_times_text

Monthly_results <- paste0(wins_text,"\n",
               fastest_time_text,"\n\n",
               avg_times_text)
Monthly_results

if (month(fastest_time$date) == last_month) {
  POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
       encode = "json",
       body =  list(text = Monthly_results,
                    type = "mrkdwn"),
       verbose()
  )
}

