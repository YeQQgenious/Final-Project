suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  })

limit_rows   <- 50000

if (!dir.exists("data")) dir.create("data")

get_311_data <- function(limit = 50000) {
  url <- paste0(
    "https://data.cityofchicago.org/resource/v6vf-nfxy.json?$limit=",
    limit
  )
  dat <- fromJSON(url, flatten = TRUE)
  as_tibble(dat)
}

message("Step 1: Retrieving data from the Chicago 311 API")

raw_311 <- get_311_data(limit_rows)

write_csv(raw_311, "data/raw_311.csv")
message("The original data has been saved to data/raw_311.csv")

glimpse(raw_311)