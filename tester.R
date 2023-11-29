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
textgraph <- final_results |> 
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  mutate(factor = 7/as.numeric(max(seconds) )) |> 
  mutate(relative = as.integer( factor*as.numeric(seconds))) |> 
  mutate(graph = strrep("━",relative)) |> 
  mutate(blanks = 8-relative) |> 
  mutate(blank = strrep("　",blanks)) |> 
  mutate(chart = paste0(graph,blank))

nyt_leaderboard_text1 <- textgraph |> 
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
  select(name,time,rank, emoji_rank, streak_text,chart) |> 
  mutate(nametime = paste0(name,": ",time," ",emoji_rank," ",streak_text,"\n")) |> 
  select(nametime) 
Results <- paste0("*",final_results_date_text,"*",
                  "\n",
                  paste0(nyt_leaderboard_text1$nametime, 
                         collapse = ""))

# plot ----
plot_data <- textgraph |> 
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  mutate(rank = min_rank(period)) |> 
  arrange(period) |> 
  mutate(emoji_rank = case_when(
    rank == 1 ~ "1st",
    rank == 2 ~ "2nd",
    rank == 3 ~ "3rd",
    .default = ""
  )) |> 
  mutate(streak_text = if_else(rank == 1,
                               if_else(streak > 3,
                                       paste0("(",streak,"-day streak)"),
                                       ""
                               )
                               ,
                               "")) |> 
 # select(name,time,rank, emoji_rank, streak_text,chart) |> 
  mutate(name_medal = ifelse(rank <= 3,
                             paste0(name,"\n",emoji_rank),
                             name))

plot <- ggplot(plot_data,
               aes(x = seconds,
                   y = fct_rev(fct_reorder( name_medal,seconds)))) +
  geom_col(fill = "#6E92E0",
           color = "#6E92E0",
           width = .75) +
  geom_text(aes(x = seconds,
                label = time),
            hjust = 1.15,
            color = "white") +
  geom_text(aes(x = seconds,
                label = streak_text),
            hjust = -.1,
            color = "black") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  )
plot 

players <- length(plot_data$name)
height = 3/7*players

file <- tempfile( fileext = ".png")
ggsave( file, plot = plot, device = "png", 
        bg = "white",
        width = 3, height = height,
        dpi = 320)

# post data ----
if (final_results_date == today) {
  POST(url =  Sys.getenv("SLACK_TEST_URL"),
       encode = "json",
       body =  list(text = Results,
                    type = "mrkdwn")
  )
}
slackr_upload(channels = "#test",
              token = Sys.getenv("SLACK_TOKEN"),
              title = final_results_date_text, 
              filename = file)

if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf")
}


