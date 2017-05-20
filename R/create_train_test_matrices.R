library(magrittr)
library(tidyverse)

source("R/utilities.R")
load("output/preprocess_train_and_test_complete.RData")

### Remove the ids and the timestamps
id_test <- cleaned_test$id

cleaned_train %<>% select(-c(timestamp, id))
cleaned_test %<>% select(-c(timestamp, id))

### Optional: remove super-correlated predictors
isnum <- cleaned_train %>% map_lgl(is.numeric)
isch <- cleaned_train %>% map_lgl(is.character)
stopifnot(sum(isch) + sum(isnum) == ncol(cleaned_train))
X <- as.matrix(cleaned_train[isnum])
cormat <- cor(X, use = "complete")
nms <- caret::findCorrelation(cormat, cutoff = 0.95, names = TRUE)
X <- X[, -match(nms, colnames(X))]
nz <- caret::nearZeroVar(X, names = TRUE)
nms <- c(nms, nz)

cleaned_train %<>% select(-one_of(nms))
cleaned_test %<>% select(-one_of(nms))

### Remove the peaks around 1M, 2M and 3M
cleaned_train$price <- y_train
cleaned_train %<>% filter(price > 1e06)
# cleaned_train %<>% filter(!price %in% c(1e06, 2e06, 3e06))
y_train <- cleaned_train$price
cleaned_train$price <- NULL

### Create a validation set
q <- quantile(cleaned_train$week, 0.9)
idx_train <- cleaned_train$week <= q
idx_val <- cleaned_train$week > q

tr <- cleaned_train[idx_train, ]
y_tr <- y_train[idx_train]
val <- cleaned_train[idx_val, ]
y_val <- y_train[idx_val]

### Create matrices
X_train <- add_dummies(cleaned_train)
X_test <- add_dummies(cleaned_test)
X_tr <- add_dummies(tr)
X_val <- add_dummies(val)

### Not all the columns are present in all the datasets
common_cols <- Reduce(intersect, list(
  colnames(X_train), 
  colnames(X_test), 
  colnames(X_tr), 
  colnames(X_val)
  )
)

X_train <- X_train[, common_cols]
X_test <- X_test[, common_cols]
X_tr <- X_tr[, common_cols]
X_val <- X_val[, common_cols]

save(
  id_test,
  y_train, 
  y_tr,
  y_val,
  X_train,
  X_test,
  X_tr,
  X_val,
  file = "output/create_train_test_matrices.RData"
)