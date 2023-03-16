options(install.packages.check.source = "no")

install.packages("tidyverse")
install.packages("httr")
install.packages("rvest")
install.packages("slackr")

update.packages(ask = FALSE,
                checkBuilt = TRUE)

