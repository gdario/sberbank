library(magrittr)
library(tidyverse)

source("R/clean_dataset.R")

train <- readr::read_csv("data/train.csv.gz")
test <- readr::read_csv("data/test.csv.gz")

### Save this for later
id_test <- test$id

ntr <- nrow(train)
nts <- nrow(test)

### Add an empty price_doc field to the test set
test$price_doc <- NA

train_test <- rbind(train, test)
timestamp_train_test <- train_test$timestamp

preprocessed_train_test <- clean_dataset(train_test)

cleaned_train <- preprocessed_train_test %>%
  map(function(x) return(x[1:ntr, ]))
cleaned_test <- preprocessed_train_test %>% 
  map(function(x) return(x[(ntr + 1):(ntr + nts), ]))

### Remove the price_doc column from cleaned_test
idx <- match("price_doc", colnames(cleaned_test$essential))
cleaned_test$essential <- cleaned_test$essential[, -idx]

timestamp_train <- timestamp_train_test[1:ntr]
timestamp_test <- timestamp_train_test[(ntr + 1):(ntr + nts)]

save(
  id_test,
  timestamp_train,
  timestamp_test,
  cleaned_train, 
  cleaned_test, 
  file = "output/preprocess_train_and_test.RData"
)