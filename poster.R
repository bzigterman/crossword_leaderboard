library(httr)
library(tidyverse)
library(slackr)
library(googlesheets4)

token <- paste0("{
  \"type\": \"service_account\",
  \"project_id\": \"",Sys.getenv("PROJECT_ID"),"\",
  \"private_key_id\": \"",Sys.getenv("PRIVATE_KEY_ID"),"\",
  \"private_key\": \"",Sys.getenv("PRIVATE_KEY"),"\",
  \"client_email\": \"",Sys.getenv("CLIENT_EMAIL"),"\",
  \"client_id\": \"",Sys.getenv("CLIENT_ID"),"\",
  \"auth_uri\": \"https://accounts.google.com/o/oauth2/auth\",
  \"token_uri\": \"https://oauth2.googleapis.com/token\",
  \"auth_provider_x509_cert_url\": \"https://www.googleapis.com/oauth2/v1/certs\",
  \"client_x509_cert_url\": \"",Sys.getenv("CLIENT_X509_CERT_URL"),"\"
}
")

write(x = token,
      file = ".secrets/token.json",
      sep = "")
gs4_auth(path = ".secrets/token.json",
         email = Sys.getenv("GOOGLE_EMAIL"))

old_csv <- read_sheet(Sys.getenv("SHEET_ID"),
                      col_types = "ccD")

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
             incoming_webhook_url = Sys.getenv("SLACK_TEST_URL"))
}

