library(magrittr)
library(tidyverse)
library(xgboost)
load("output/create_dummies.RData")

y <- train$price_doc
train %<>% select(-one_of("price_doc"))

### Utility functions for missing value counting
sum_nas <- function(x) 
  sum(is.na(x))
prop_nas <- function(x)
  sum_nas(x) / length(x)

### Removing the missing values seems to damage the performance
# missing_train <- sapply(train, prop_nas)
# missing_test <- sapply(test, prop_nas)
# 
# missing_train <- sort(missing_train[missing_train > 0])
# missing_test <- sort(missing_test[missing_test > 0])
# 
# too_many_na_train <- which(missing_train > 0.25)
# too_many_na_test <- which(missing_test > 0.25)

# train %<>% select(-one_of(names(too_many_na_train)))
# test %<>% select(-one_of(names(too_many_na_test)))

common_cols <- intersect(names(train), names(test))

### Remove life_sq from the common columns due to the extreme number
### of missing values, and its correlation with full_sq
common_cols <- common_cols[-match('life_sq', common_cols)]

train <- train[common_cols]
test <- test[common_cols]

idx_train <- train$week < quantile(train$week, 0.8)
idx_val <- !idx_train

X_train <- as.matrix(train[idx_train, ])
y_train <- log(y[idx_train])

X_val <- as.matrix(train[idx_val, ])
y_val <- log(y[idx_val])

dTrain <- xgb.DMatrix(data = X_train, label = y_train)
dVal <- xgb.DMatrix(data = X_val, label = y_val)

watchlist <- list(eval = dVal, train = dTrain)

params <- list(
  eta = 0.1,
  nthread = 4,
  objective = 'reg:linear',
  eval_metric = 'rmse',
  gamma = 0,
  max_depth = 10
)

res <- xgb.train(
  params = params,
  data = dTrain,
  nrounds = 100,
  watchlist = watchlist,
  early_stopping_rounds = 20
)

nms <- colnames(dTrain)
importance_matrix <- xgb.importance(nms, model = res)
# xgb.plot.importance(importance_matrix[1:100, ])

features <- importance_matrix$Feature[1:100]

X_train <- X_train[, features]
X_val <- X_val[, features]
X_test <- as.matrix(test[features])

save(X_train, y_train, X_val, y_val, id_test, X_test,
  file = "output/select_features.RData")