library(xgboost)

load("output/select_features.RData")

dTrain <- xgb.DMatrix(data = X_train, label = y_train)
dVal <- xgb.DMatrix(data = X_val, label = y_val)
dTest <- xgb.DMatrix(data = X_test)

dTrainComplete <- xgb.DMatrix(
  data = rbind(X_train, X_val),
  label = c(y_train, y_val)
)

watchlist <- list(eval = dVal, train = dTrain)

params <- list(
  eta = 0.1,
  nthread = 4,
  objective = 'reg:linear',
  eval_metric = 'rmse',
  gamma = 0,
  max_depth = 5
)

res <- xgb.train(
  params = params,
  data = dTrain,
  nrounds = 130,
  watchlist = watchlist,
  early_stopping_rounds = 20
)

resComplete <- xgb.train(
  data = dTrainComplete, 
  params = params,
  nrounds = 130
)

predictions <- round(exp(predict(resComplete, dTest)), 2)
submission <- tibble(id = id_test, price_doc = predictions)

write_csv(submission, path = "submission2.csv")
