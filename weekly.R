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
                      col_types = "TccD") |> 
  filter(date > today(tzone = "America/Chicago")-days(90)) |> 
  mutate(week = isoweek(date)) 

current_week <- isoweek(now(tzone = "America/Chicago"))
last_week <- isoweek(now(tzone = "America/Chicago")-weeks(1))

old_with_ranks <- old_csv |>
  mutate(period = ms(time)) |> 
  mutate(seconds = seconds(period)) |> 
  group_by(date) |> 
  arrange(period) |> 
  arrange(date) |> 
  mutate(rank = min_rank(period)) |> 
  ungroup() |> 
  filter(week == current_week) |> 
  select(!week) 

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

wins_text <- paste0("*Week ",current_week," wins*",
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
  mutate(minutes = as.numeric(seconds/60))|> 
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
  #              fill = "#e2e9f8")+
  geom_point(color = "#6E92E0",
             shape = 21,
             stroke = .5,
             size = 2.5)+
  geom_segment(aes(x = min(minutes),
                   xend = max(minutes),
                   y = 0,
                   yend = 0),
               color = "black") +
  scale_fill_manual(values = c("#6E92E07F","#E6B83D7F"),
                    guide = NULL) +
  theme_minimal()+
  ylab(NULL)+
  ggtitle(paste0("Week ",current_week)) +
  theme(panel.grid  = element_blank(),
        axis.ticks.x = element_line() )
plot

times_plot <- tempfile( fileext = ".png")
ggsave( times_plot, plot = plot, device = "png", 
        bg = "white",
        width = 3, height = 3,
        dpi = 640)

if (isoweek(old_with_ranks$date[[1]]) == current_week) {
  # POST(url =  Sys.getenv("SLACK_CROSSWORD_URL"),
  #      encode = "json",
  #      body =  list(text = wins_text,
  #                   type = "mrkdwn")
  # )
  
  slackr_upload(channels = "#test",
                initial_comment = wins_text,
                token = Sys.getenv("SLACK_TOKEN"),
                title = paste0("Week ",current_week), 
                filename = times_plot)
}

if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf")
}

