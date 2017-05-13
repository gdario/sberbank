create_submission <- function(X_train=NULL, 
                              y_train=NULL, 
                              X_test=NULL,
                              eta=NULL,
                              max_depth=NULL,
                              gamma=NULL,
                              nrounds=NULL,
                              colsample_bytree = NULL,
                              min_child_weight = NULL,
                              subsample = NULL) {
  require(xgboost)

  params <- list(
    eta = eta,
    nthread = 4,
    objective = 'reg:linear',
    eval_metric = 'rmse',
    gamma = gamma,
    min_child_weight = min_child_weight,
    subsample = subsample,
    max_depth = max_depth
  )
  
  dTrain <- xgb.DMatrix(X_train, label = y_train)
  dTest <- xgb.DMatrix(X_test)
  
  resComplete <- xgb.train(
    data = dTrain,
    params = params,
    nrounds = nrounds
  )
  
  predictions <- round(exp(predict(resComplete, dTest)), 2)
  submission <- tibble::tibble(id = id_test, price_doc = predictions)
  sub_name <- paste0("output/", gsub("[ :]", "_", date()), ".csv")
  readr::write_csv(submission, path = sub_name)
}