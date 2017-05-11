library(tidyverse)

train <- readr::read_csv("data/train.csv.gz")
test <- readr::read_csv("data/test.csv.gz")
macro <- readr::read_csv("data/macro.csv.zip")

### Merge train and macro by timestamp
train_macro <- inner_join(train, macro, by = "timestamp")
test_macro <- inner_join(test, macro, by = "timestamp")

### Extract year and month from the timestamp
train_macro$year <- lubridate::year(train_macro$timestamp)
train_macro$month <- lubridate::month(train_macro$timestamp)

test_macro$year <- lubridate::year(test_macro$timestamp)
test_macro$month <- lubridate::month(test_macro$timestamp)

save(train_macro, test_macro, file = "output/merge_with_macro.RData",
  compress = TRUE)