library(magrittr)
library(tidyverse)
source("R/clean_dataset.R")

match_timestamps <- function(ts1, ts2) {
  df1 <- data_frame(ts = ts1, idx_dataset = seq_along(ts1))
  df2 <- data_frame(ts = ts2, idx_macro = seq_along(ts2))
  df <- inner_join(df1, df2)
  df
}

macro <- readr::read_csv("data/macro.csv.zip")

timestamp_macro <- macro$timestamp
macro %<>% select(-timestamp)

### 
fix_separator <- function(x) {
  as.numeric(sub(",", ".", x))
}

macro %<>% mutate(
  child_on_acc_pre_school = fix_separator(child_on_acc_pre_school),
  modern_education_share = fix_separator(modern_education_share),
  old_education_build_share = fix_separator(old_education_build_share)
)

macro <- as.matrix(macro)

### Discard the columns with more than 30% of NAs
perc_na <- apply(macro, 2, function(x) mean(is.na(x)))
idx_na <- perc_na < 0.3

macro <- macro[, idx_na]

### After applying findCorrelation we end up with a vector of names
### to remove. This is stored in data/remove_macro.txt
to_remove <- scan("data/remove_macro.txt", sep = "\t", what = "")
idx <- match(to_remove, colnames(macro))
macro <- macro[, -idx]

### Combine with the cleaned training and test sets
load("output/preprocess_train_and_test.RData")

mapping_train <- match_timestamps(timestamp_train, timestamp_macro)
macro_train <- macro[mapping_train$idx_macro, ]

mapping_test <- match_timestamps(timestamp_test, timestamp_macro)
macro_test <- macro[mapping_test$idx_macro, ]

cleaned_train$macro <- macro_train
cleaned_test$macro <- macro_test

save(
  id_test,
  timestamp_train,
  timestamp_test,
  cleaned_train,
  cleaned_test,
  file = "output/preprocess_macro.RData"
)