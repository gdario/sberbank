##################################################################
### This script tries to emulate the results described by Bruno
### Do Amaral in this kernel
### https://www.kaggle.com/bguberfain/naive-xgb-lb-0-317
##################################################################
library(magrittr)
library(tidyverse)
library(lubridate)
library(xgboost)


### Select these variables from the `macro` dataset as suggested by
### https://www.kaggle.com/robertoruiz/sberbank-russian-housing-market/dealing-with-multicollinearity/notebook
macro_cols = c(
    "balance_trade",
    "balance_trade_growth",
    "eurrub",
    "average_provision_of_build_contract",
    "micex_rgbi_tr",
    "micex_cbi_tr",
    "deposits_rate",
    "mortgage_value",
    "mortgage_rate",
    "income_per_cap",
    "rent_price_4+room_bus",
    "museum_visitis_per_100_cap",
    "apartment_build"
)

df_train <- read_csv("data/train.csv.gz")
df_test  <- read_csv("data/test.csv.gz")
df_macro <- read_csv("data/macro.csv.zip")

### Restrict the macro dataset to the columns above, plus the
### timestamp
df_macro %<>% select(one_of('timestamp', macro_cols))

ylog_train_all <- log(1 + df_train$price_doc)
id_test <- df_test$id
df_test$id <- NULL
num_train = nrow(df_train)
df_train %<>% select(-c(id, price_doc))

### Some classes differ between the two datasets
class_train <- df_train %>% map_chr(class)
class_test <- df_test %>% map_chr(class)
stopifnot(all.equal(names(class_train), names(class_test)))
different_class <- which(class_train != class_test)

df_train %<>% mutate(max_floor = as.integer(max_floor),
                     material = as.integer(material),
                     build_year = as.integer(build_year),
                     num_room = as.integer(num_room),
                     kitch_sq = as.integer(kitch_sq),
                     state = as.integer(state))

df_all = bind_rows(df_train, df_test)
df_all %<>% left_join(df_macro, by = 'timestamp')

### Add month-year, week-year, day-of-week
df_all %<>% mutate(month_year  = 100 * year(timestamp) +
                       month(timestamp),
                   week_year = 100 * year(timestamp) +
                       (week(timestamp)),
                   dow = wday(timestamp) - 1,
                   month = month(timestamp),
                   rel_floor = floor / as.numeric(max_floor),
                   rel_kitch_sq = kitch_sq / as.numeric(full_sq))

### Count the number of transactions in each month-year
month_year_cnt_map <- df_all %>%
    select(month_year) %>%
    group_by(month_year) %>%
    summarise(month_year_cnt = n())

### Count the number of transactions in each week-year
week_year_cnt_map <- df_all %>%
    select(week_year) %>%
    group_by(week_year) %>%
    summarise(week_year_cnt = n())

timestamp_all <- df_all$timestamp
df_all %<>% left_join(month_year_cnt_map)
df_all %<>% left_join(week_year_cnt_map)
df_all %<>% select(-c(month_year, week_year, timestamp))

### Deal with categorical values
numeric_vars <- names(df_all)[map_lgl(df_all, is.numeric)]
df_numeric = df_all[numeric_vars]

### Categorical variables
categorical_vars <- names(df_all)[map_lgl(df_all, is.character)]
df_obj = df_all[categorical_vars]

### The pd.factorize function seems to be the equivalent of
### `as.numeric(as.factor))`
factorize <- function(x) {
  lv <- unique(x)
  f <- factor(x, levels = lv)
  as.numeric(f) - 1
}

df_obj %<>% map_df(factorize)
df_values <- bind_cols(df_numeric, df_obj)
# df_values <- cbind(as.numeric(timestamp_all), df_values)

## ### Convert to matrices
X_all <- as.matrix(df_values)

## ### Create a validation set with the last 20% of the data
num_val <- trunc(num_train * 0.2)

X_train_all <- X_all[1:num_train, ]
X_train <- X_all[seq(1, num_train - num_val), ]
ylog_train <- ylog_train_all[seq(1, num_train - num_val)]

X_val <- X_all[seq(num_train - num_val + 1, num_train), ]
ylog_val <- ylog_train_all[seq(num_train - num_val + 1, num_train)]

X_test <- X_all[seq(num_train + 1, nrow(X_all)), ]

### Create an xgboost dataset
dtrain_all <- xgb.DMatrix(data = X_train_all, label = ylog_train_all)
dtrain <- xgb.DMatrix(data = X_train, label = ylog_train)
dval <- xgb.DMatrix(data = X_val, label = ylog_val)
dtest <- xgb.DMatrix(data = X_test)

xgb_params <- list(
   eta = 0.05,
   max_depth = 5,
   subsample = 1,
   colsample_bytree = .7,
   objective = "reg:linear",
   eval_metric = "rmse",
   silent = 1
)

watch_list <- list(eval = dval, train = dtrain)

partial_model <- xgb.train(
   params = xgb_params,
   data = dtrain,
   nrounds = 500,
   watchlist = watch_list,
   maximize = FALSE)

num_boost_round <- which.min(partial_model$evaluation_log$eval_rmse)
model <- xgb.train(params = xgb_params, data = dtrain_all,
                    nrounds = num_boost_round)

importance <- xgb.importance(model, feature_names = colnames(dtrain_all))
xgb.plot.importance(importance, top_n = 50)

ylog_pred <- predict(model, newdata = dtest)
y_pred <- exp(ylog_pred)
df_sub <- tibble(id = id_test, price_doc = y_pred)

## # write_csv(df_sub, path = "output/bruno_do_amaral.csv")
