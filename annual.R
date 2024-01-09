library(httr)
library(tidyverse)
library(slackr)
library(googlesheets4)
library(ggbeeswarm)

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

# fastest times ----
fastest_time <- old_with_ranks |> 
  arrange(period) |> 
  mutate(date_text = strftime(x = date, 
                              tz = "US/Central",
                              format = "%a, %b %d")) |> 
  mutate(timenamedate = paste0(time," by ",name," on ",date_text,"\n")) |> 
  mutate(minutes = as.numeric(seconds/60)) |> 
  mutate(weekday = wday(date)) |> 
  mutate(saturday_check = case_when(
    weekday == 7 ~ "Saturday",
    .default = "Non-Saturday"
  ))

top_three <- fastest_time |> 
  head(n = 3) 

fastest_time_text <- paste0("*Fastest times*\n",
                            paste0(top_three$timenamedate, 
                                   collapse = ""))

plot <- ggplot(fastest_time,
               aes(x = minutes,
                   fill = saturday_check,
                   y = fct_rev( fct_reorder( name,minutes))))+
  # geom_boxplot(color = "#6E92E0",
  #              outlier.size = 0,
  #              fill = "#e2e9f8")+
  geom_quasirandom(color = "#6E92E0",
                   shape = 21,
                   stroke = 0.075,
                   size = 1)+
  # geom_beeswarm(color = "#579C49",
  #               method = "square",
  #               cex = .5,
  #               size = .2)+
  #geom_violin()+
  # geom_jitter(color = "#6E92E0",
  #             height = .3,
  #             size = .2) +
  geom_segment(aes(x = min(minutes),
                   xend = max(minutes),
                   y = 0,
                   yend = 0),
               color = "black") +
  scale_fill_manual(values = c("#6E92E07F","#E6B83D7F"),
                    guide = NULL) +
  theme_minimal()+
  ylab(NULL)+
  theme(panel.grid  = element_blank(),
        axis.ticks.x = element_line())
plot

times_plot <- tempfile( fileext = ".png")
ggsave( times_plot, plot = plot, device = "png", 
        bg = "white",
        width = 3, height = 3,
        dpi = 640)


avg_times <- old_with_ranks |> 
  group_by(name) |> 
  mutate(avg = round(median(as.numeric( seconds))) )|> 
  ungroup() |> 
  select(name, avg) |> 
  distinct() |> 
  arrange(avg) |> 
  mutate(avg_seconds = seconds(avg)) |> 
  mutate(avg_period = seconds_to_period(avg_seconds)) |> 
  mutate(avg_text = sprintf("%d:%02d",minute(avg_period), second(avg_period))) |> 
  mutate(name_avg = paste0(name,": ",avg_text,"\n")) 

avg_times_text <- paste0("*Median times*\n",
                         paste0(avg_times$name_avg, 
                                collapse = ""))


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
  mutate(start = if_else(is.na(lag),
                         TRUE,
                         if_else(lag == date-days(1),
                                 FALSE,
                                 TRUE))) |> 
  mutate(start_id = if_else(start,1,0)) |> 
  mutate(streak_id = cumsum(start_id)) |> 
  group_by(name,streak_id) |> 
  arrange(name,streak_id) |> 
  mutate(streak = row_number()) |> 
  ungroup() |> 
  select(name, date, streak)

streaks <- old_adding_streaks

old_csv_with_streaks <- full_join(old_csv, streaks) |> 
  mutate(year = year(date)) |> 
  filter(year == last_year) |> 
  arrange(desc(streak)) |> 
  mutate(rank = min_rank(desc(streak))) |> 
  filter(rank == 1) |> 
  mutate(date_text = strftime(x = date, 
                              tz = "US/Central",
                              format = "%a, %b %d")) |>
  mutate(text = paste0(name,": ",streak,", ending on ",date_text,"\n"))

longest_streak <- max(old_csv_with_streaks$streak)

longest_streak_text <- if_else(longest_streak > 2,
                               paste0("*Longest streak*\n",
                                      paste0(old_csv_with_streaks$text, 
                                             collapse = "")),
                               "")

# personalized ----
mine <- old_csv |> 
  mutate(year = year(date)) |> 
  filter(year == last_year) |> 
  #filter(name == "Ben") |> 
  mutate(weekday = as.character( wday(date)) )|> 
  mutate(weekday = case_match(
    weekday,
    "1" ~ "M",
    "2" ~ "Tu",
    "3" ~ "W",
    "4" ~ "Th",
    "5" ~ "F",
    "6" ~ "Sa",
    "7" ~ "Su"
  )) |> 
  mutate(weekday = fct_relevel(weekday, "M","Tu","W","Th","F","Sa","Su")) |> 
  mutate(week = isoweek(date))|>
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  mutate(minutes = as.numeric(seconds/60)) |> 
  mutate(saturday_check = case_when(
    weekday == 7 ~ "Saturday",
    .default = "Non-Saturday"
  )) |> 
  group_by(date) |> 
  arrange(period) |> 
  arrange(date) |> 
  mutate(rank = as_factor( min_rank(period))) |> 
  ungroup() 

weekday_medians <- mine |> 
  group_by(name,weekday) |> 
  mutate(weekday_median = median(minutes)) |> 
  ungroup() |> 
  select(name,weekday, weekday_median) |> 
  distinct()

plot <- ggplot(weekday_medians)+
  geom_col(aes(x = weekday,
               y = weekday_median),
           fill = "#6E92E0",
           color = "white")+
  theme_minimal()+
  ylab("Median time")+
  xlab(NULL)+
  theme(panel.grid.minor.x  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        axis.ticks.x = element_line()) +
  facet_wrap(~name)
plot
weekday_median_plot <- tempfile( fileext = ".png")
ggsave( weekday_median_plot, plot = plot, device = "png", 
        bg = "white",
        width = 3, height = 3,
        dpi = 640)


plot <- ggplot(mine,
               aes(x = rank))+
  stat_count( fill = "#6E92E0",
              color = "white") +
  facet_wrap(~name)+
  xlab("Rank")+
  ylab("Count") +
  theme_minimal()+
  theme(panel.grid.minor.x  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        axis.ticks.x = element_line())
plot
ranks_plot <- tempfile( fileext = ".png")
ggsave( ranks_plot, plot = plot, device = "png", 
        bg = "white",
        width = 3, height = 3,
        dpi = 640)


# combined ----

Annual_results <- paste0(wins_text,"\n",
                         fastest_time_text,"\n",
                         avg_times_text,"\n",
                         longest_streak_text)

if (year(fastest_time$date[[1]]) == last_year) {
  # POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
  #      encode = "json",
  #      body =  list(text = Annual_results,
  #                   type = "mrkdwn")
  # )
  
  slackr_upload(channels = "#test",
                initial_comment = wins_text,
                token = Sys.getenv("SLACK_TOKEN"),
                title = paste0(last_year_text), 
                filename = ranks_plot )
  slackr_upload(channels = "#test",
                initial_comment = avg_times_text,
                token = Sys.getenv("SLACK_TOKEN"),
                title = paste0(last_year_text), 
                filename = weekday_median_plot)
  slackr_upload(channels = "#test",
                initial_comment = paste0(fastest_time_text,"\n",
                                         longest_streak_text),
                token = Sys.getenv("SLACK_TOKEN"),
                title = paste0(last_year_text), 
                filename = times_plot)
  
}

if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf")
}

