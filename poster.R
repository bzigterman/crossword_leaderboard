library(httr)
library(tidyverse)
library(slackr)

old_csv <- read_csv("leaderboard.csv",
                    col_types = cols(
                      time = col_character()
                    )) 

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

if (final_results_date == today) {
  slackr_bot(Results,
             incoming_webhook_url = Sys.getenv("SLACK_CROSSWORD_URL"))
}
