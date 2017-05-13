library(caret)
library(xgboost)
load("output/create_val.RData")
source("R/create_submission.R")

### We need to manually create the indexes for the lagged CV
N <- 20741 # Magic number - training set size

idx_train <- lapply(0:4, function(k) {
  m <- k * 2000
  seq(m + 1, m + N)
})

idx_val <- lapply(0:4, function(k) {
  seq(N + k * 2000 + 1, N + (k + 1) * 2000)
})

X_train <- with(full_train, cbind(
  essential, count, km, radius, macro
))
y_train <- log(1 + full_train$y)

X_test <- with(test, cbind(
  essential, count, km, radius, macro
))

### Let's create a (rather arbitrary) grid of parameters
xgbGrid <- expand.grid(
  nrounds = c(2000, 5000),
  max_depth = c(1, 3, 5),
  eta = 0.01,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

xgbCtrl <- trainControl(
  method = "cv",
  index = idx_train,
  indexOut = idx_val,
  repeats = 1,
  verboseIter = TRUE
)

xgbTrain <- train(
  x = X_train,
  y = y_train,
  objective = "reg:linear",
  trControl = xgbCtrl,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

create_submission(
  X_train = X_train,
  y_train = y_train,
  X_test = X_test,
  eta = 0.01,
  gamma = 0,
  max_depth = 3,
  nrounds = 2000,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
