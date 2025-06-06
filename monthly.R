library(httr)
library(tidyverse)
library(slackr)
library(googlesheets4)
library(ggbeeswarm)

# Google API ----
json <- Sys.getenv("TOKEN_KEY") |>
  stringr::str_replace_all(pattern = fixed("\\n"), replacement = "\n")
dec <- rawToChar(jsonlite::base64_dec(json))

gs4_auth(path = dec)

# read data ----
old_csv <- read_sheet(
  ss = Sys.getenv("SHEET_ID"),
  sheet = "Form Responses 1",
  col_types = "TccD"
) |>
  filter(date > today(tzone = "America/Chicago") - days(90))

current_month <- month(now(tzone = "America/Chicago"))
last_month <- if_else(current_month == 1, 12, current_month - 1)
last_month_text <- strftime(
  x = ymd(paste("2023-", last_month, "-01")),
  tz = "US/Central",
  format = "%B"
)

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

# wins ----
wins <- old_with_ranks |>
  filter(rank == 1) |>
  select(!date) |>
  count(name) |>
  arrange(desc(n)) |>
  select(name, n) |>
  mutate(rank = min_rank(desc(n))) |>
  mutate(emoji_rank = if_else(rank == 1, ":crown:", "")) |>
  mutate(name_text = paste0(name, ": ", n, " ", emoji_rank, "\n"))

wins_text <- paste0(
  "*",
  last_month_text,
  " results*\n\n*Wins*",
  "\n",
  paste0(wins$name_text, collapse = "")
)

# fastest times ----
fastest_time <- old_with_ranks |>
  arrange(period) |>
  mutate(
    date_text = strftime(x = date, tz = "US/Central", format = "%a, %b %d")
  ) |>
  mutate(timenamedate = paste0(time, " by ", name, " on ", date_text, "\n")) |>
  mutate(minutes = as.numeric(seconds / 60)) |>
  mutate(weekday = wday(date)) |>
  mutate(
    saturday_check = case_when(
      weekday == 7 ~ "Saturday",
      .default = "Non-Saturday"
    )
  )

top_three <- fastest_time |>
  head(n = 3)

fastest_time_text <- paste0(
  "*Fastest times*\n",
  paste0(top_three$timenamedate, collapse = "")
)

plot <- ggplot(
  fastest_time,
  aes(
    x = minutes,
    fill = saturday_check,
    y = fct_rev(fct_reorder(name, minutes))
  )
) +
  geom_quasirandom(color = "#6E92E0", shape = 21, stroke = .25, size = 1.75) +
  geom_segment(
    aes(x = min(minutes), xend = max(minutes), y = 0, yend = 0),
    color = "black"
  ) +
  scale_fill_manual(values = c("#6E92E07F", "#E6B83D7F"), guide = NULL) +
  theme_minimal() +
  ylab(NULL) +
  ggtitle(paste0(last_month_text)) +
  theme(panel.grid = element_blank(), axis.ticks.x = element_line())
plot

times_plot <- tempfile(fileext = ".png")
ggsave(
  times_plot,
  plot = plot,
  device = "png",
  bg = "white",
  width = 3,
  height = 3,
  dpi = 640
)

# avg times ----
avg_times <- old_with_ranks |>
  group_by(name) |>
  mutate(avg = round(median(as.numeric(seconds)))) |>
  ungroup() |>
  select(name, avg) |>
  distinct() |>
  arrange(avg) |>
  mutate(avg_seconds = seconds(avg)) |>
  mutate(avg_period = seconds_to_period(avg_seconds)) |>
  mutate(
    avg_text = sprintf("%d:%02d", minute(avg_period), second(avg_period))
  ) |>
  mutate(name_avg = paste0(name, ": ", avg_text, "\n"))

avg_times_text <- paste0(
  "*Median times*\n",
  paste0(avg_times$name_avg, collapse = "")
)

# longest streak ----
old_adding_streaks <- old_csv |>
  mutate(period = ms(time)) |>
  mutate(seconds = seconds(period)) |>
  group_by(date) |>
  arrange(period) |>
  arrange(date) |>
  mutate(rank = min_rank(period)) |>
  ungroup() |>
  filter(rank == 1) |>
  select(name, date) |>
  group_by(name) |>
  mutate(lag = lag(date)) |>
  mutate(
    start = if_else(
      is.na(lag),
      TRUE,
      if_else(lag == date - days(1), FALSE, TRUE)
    )
  ) |>
  mutate(start_id = if_else(start, 1, 0)) |>
  mutate(streak_id = cumsum(start_id)) |>
  group_by(name, streak_id) |>
  arrange(name, streak_id) |>
  mutate(streak = row_number()) |>
  ungroup() |>
  select(name, date, streak)

streaks <- old_adding_streaks

old_csv_with_streaks <- full_join(old_csv, streaks) |>
  mutate(month = month(date)) |>
  filter(month == last_month) |>
  arrange(desc(streak)) |>
  mutate(rank = min_rank(desc(streak))) |>
  filter(rank == 1) |>
  mutate(
    date_text = strftime(x = date, tz = "US/Central", format = "%a, %b %d")
  ) |>
  mutate(text = paste0(name, ": ", streak, ", ending on ", date_text, "\n"))

longest_streak <- max(old_csv_with_streaks$streak)

longest_streak_text <- if_else(
  longest_streak > 2,
  paste0(
    "*Longest streak*\n",
    paste0(old_csv_with_streaks$text, collapse = "")
  ),
  ""
)

# combine ----
Monthly_results <- paste0(
  wins_text,
  "\n",
  fastest_time_text,
  "\n",
  avg_times_text,
  "\n",
  longest_streak_text
)

if (month(fastest_time$date[[1]]) == last_month) {
  # POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
  #      encode = "json",
  #      body =  list(text = Monthly_results,
  #                   type = "mrkdwn")
  # )

  slackr_upload(
    channels = "#crossword",
    initial_comment = Monthly_results,
    token = Sys.getenv("SLACK_TOKEN"),
    title = paste0(last_month_text),
    filename = times_plot
  )
}

if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf")
}
