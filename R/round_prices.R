library(caret)
library(magrittr)
library(tidyverse)
library(xgboost)

load("output/08-final_dataset.RData")

prices %<>% mutate(
  round_prices = ifelse(price_doc %in% c(1e6, 2e6, 3e6), 1, 0)
) %>% select(-timestamp)

train_set %<>% filter(
  product_typeOwnerOccupier == 0
)

X <- inner_join(prices, train_set, by = "id") %>% 
  select(-c(id, price_doc, timestamp))

X0 <- filter(X, round_prices == 0) %>% sample_n(2000)
X1 <- filter(X, round_prices == 1)

X <- bind_rows(X0, X1)
y <- X$round_prices

tmp <- X %>% select(sub_area)
cd <- dummyVars(~ sub_area, data = tmp, fullRank = FALSE)
d <- predict(cd, tmp)

X %<>% select(
  -c(sub_area, round_prices, product_typeOwnerOccupier)
  ) %>% as.matrix()
X <- cbind(X, d)

idx <- createDataPartition(y, list = FALSE, p = 0.75)
X_train <- X[idx, ]
y_train <- y[idx]
X_val <- X[-idx, ]
y_val <- y[-idx]

dTrain <- xgb.DMatrix(X_train, label = y_train)
dTest <- xgb.DMatrix(X_val, label = y_val)

params <- list(
  eta = 0.01,
  gamma = 1,
  max_depth = 4,
  min_child_weight = 1,
  subsample = .9,
  colsample_bytree = 1,
  objective = "binary:logistic"
)

# watchlist <- list(aval = dTest, train = dTrain)

tr <- xgb.cv(
  params = params,
  data = dTrain, 
  nrounds = 300, 
  nfold = 10
)