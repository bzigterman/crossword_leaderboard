options(install.packages.check.source = "no")

install.packages("tidyverse")
install.packages("slackr")
install.packages("gargle")
install.packages("sodium")
install.packages("jsonlite")

update.packages(ask = FALSE,
                checkBuilt = TRUE)
