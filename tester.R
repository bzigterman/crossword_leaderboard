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
                               if_else(streak >= 3,
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
    rank == 1 ~ "1",
    rank == 2 ~ "2",
    rank == 3 ~ "3",
    .default = ""
  )) |> 
  mutate(streak_text = if_else(rank == 1,
                               if_else(streak >= 3,
                                       paste0(streak,"-day streak"),
                                       NA),
                               NA)) |> 
  mutate(name_medal = ifelse(rank <= 3,
                             paste0(name,"\n",emoji_rank),
                             name))

plot <- ggplot(plot_data,
               aes(x = seconds,
                   y = fct_rev(fct_reorder( name,seconds)))) +
  geom_segment(aes(x = 0,
                   xend = seconds,
                   yend = fct_rev(fct_reorder( name,seconds))),
               color = "#6E92E0") +
  geom_point(aes(color = as_factor( emoji_rank),
                 fill = as_factor( emoji_rank)),
             shape = 21,
             size = 4) +
  geom_text(aes(x = seconds,
                label = emoji_rank),
            size = 2.5,
            color = "white",
            alpha = .5)+
  geom_text(aes(x = seconds,
                label = time),
            vjust = -.8,
            hjust = "inward",
            color = "black") +
  geom_text(aes(x = seconds,
                label = streak_text),
            hjust = -.15,
            color = "black") +
  theme_minimal() +
  ylab(NULL) +
  xlab(NULL) +
  scale_color_manual(breaks = c("1","2","3",""),
                     values = c("gold","#C0C0C0","#CD7F32",
                                "#6E92E0"),
                     guide = NULL) +
  scale_fill_manual(breaks = c("1","2","3",""),
                    values = c("#ffc125","#acacac","#b8722d",
                               "#6E92E0"),
                    guide = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  )+
  ggtitle(final_results_date_text)
plot 

file <- tempfile( fileext = ".png")
ggsave( file, plot = plot, device = "png", 
        bg = "white",
        width = 3, height = 3,
        dpi = 640)

# post data ----
if (final_results_date == today) {
  # POST(url =  Sys.getenv("SLACK_TEST_URL"),
  #      encode = "json",
  #      body =  list(text = Results,
  #                   type = "mrkdwn")
  # )
  
  slackr_upload(channels = "#test",
                initial_comment = Results,
                token = Sys.getenv("SLACK_TOKEN"),
                title = "Leaderboard", 
                filename = file)
}
if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf")
}


