# library(magrittr)
# library(tidyverse)

load("output/preprocess_macro.RData")

week <- cleaned_train$essential[, "week"]
q <- quantile(week, 0.9)

idx_train <- week <= q
idx_val <- week > q

foo <- function(x, ix)
  x[ix, ]

train <- lapply(cleaned_train, foo, ix = idx_train)
val <- lapply(cleaned_train, foo, ix = idx_val)

create_y <- function(x) {
  idx <- match("price_doc", colnames(x$essential))
  y <- x$essential[, idx]
  x$essential <- x$essential[, -idx]
  x$y <- y
  x
}

train <- create_y(train)
val <- create_y(val)
test <- cleaned_test

### We also return a full version of the training set for time-dep
### cross validation

full_train <- create_y(cleaned_train)

save(id_test, full_train, train, val, test, 
     file = "output/create_val.RData")