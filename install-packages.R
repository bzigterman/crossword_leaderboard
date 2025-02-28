options(install.packages.check.source = "no")

install.packages("tidyverse")
#install.packages("slackr")
install.packages("jsonlite")
install.packages('ggbeeswarm')
install.packages("jsonlite")
install.packages("janitor")

install.packages("remotes")
remotes::install_github("mrkaye97/slackr")

update.packages(ask = FALSE, checkBuilt = TRUE)
