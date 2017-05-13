library(caret)
library(xgboost)
load("output/create_val.RData")
source("R/create_submission.R")

### We need to manually create the indexes for the lagged CV
N <- 25741 # Magic number - training set size

idx_train <- lapply(0:9, function(k) {
  m <- k * 500
  seq(m + 1, m + N)
})

idx_val <- lapply(0:9, function(k) {
 seq(N + k * 500 + 1, N + (k + 1) * 500)
})

X_train <- full_train$essential
y_train <- log(1 + full_train$y)

X_test <- test$essential

### Let's create a (rather arbitrary) grid of parameters
xgbGrid <- expand.grid(
  nrounds = c(100, 200, 500),
  max_depth = c(3, 5, 10),
  eta = 0.1,
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

### Best performance obtained with max_depth=3 and nrounds=100
create_submission(
  X_train = X_train, 
  y_train = y_train,
  X_test = X_test,
  eta = 0.1,
  gamma = 0,
  max_depth = 3, 
  nrounds = 100, 
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = 1
)
