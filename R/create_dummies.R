library(magrittr)
library(tidyverse)
load("output/clean_columns.RData")

add_dummies <- function(x) {
  ### This option is essential for the creation of dummy variables with
  ### NAs
  op <- options(na.action = "na.pass")
  idx_char <- sapply(x, is.character)
  tmp <- x[idx_char]
  not_char <- x[, !idx_char]
  nms <- names(tmp)
  for (n in nms) {
    f <- as.formula(paste("~ -1 +", n))
    d <- model.matrix(f, data = tmp)
    tmp <- cbind(tmp, d)
    tmp[[n]] <- NULL
  }
  options(op)
  cbind(not_char, tmp)
}

### TRAINING SET
train <- add_dummies(train_macro)
test <- add_dummies(test_macro)

save(
  id_train, train, id_test, test, 
  file = "output/create_dummies.RData"
)


