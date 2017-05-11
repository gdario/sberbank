library(VIM)
library(mice)

load("output/select_features.RData")
X <- rbind(X_train, X_val)
y <- c(y_train, y_val)

sum_nas <- function(x) 
  sum(is.na(x))
prop_nas <- function(x)
  sum_nas(x) / length(x)

missing_train <- apply(X, 2, prop_nas)
missing_test <- apply(X_test, 2, prop_nas)

missing_train <- sort(missing_train[missing_train > 0])
missing_test <- sort(missing_test[missing_test > 0])
