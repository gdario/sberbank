library(tidyverse)
library(magrittr)
load("output/merge_with_macro.RData")

### None of the ids in the training set is in the test set
# sum(train_macro$id %in% test_macro$id)
id_train <- train_macro$id
id_test <- test_macro$id

train_macro$id <- test_macro$id <- NULL

### The columns containing `raion` refer to large areas and are in
### general of little interest to us

train_macro %<>% select(-contains('raion'))
test_macro %<>% select(-contains('raion'))

### Let's also remove the insane number of variables staring with
### `cafe_`
train_macro %<>% select(-starts_with('cafe_'))
test_macro %<>% select(-starts_with('cafe_'))

### Remove the variables that end with a 3 or 4 digit number
train_macro %<>% select(-matches('[[:digit:]]{3,4}$'))
test_macro %<>% select(-matches('[[:digit:]]{3,4}$'))

### Remove the demographic variables
train_macro %<>% select(-c(full_all:`0_13_female`))
test_macro %<>% select(-c(full_all:`0_13_female`))

### Macro-economic indicators

### Let's start with a subset of macroeconomic indicators
sub_macro <- train_macro %>%
  select(oil_urals:apartment_fund_sqm)

to_keep <- select(sub_macro,
  gdp_quart:gdp_quart_growth, 
  gdp_annual, gdp_annual_growth,
  mortgage_value:mortgage_rate,
  fixed_basket, unemployment,
  salary, salary_growth,
  fin_res_per_cap,
  construction_value,
  starts_with('rent_price')) %>%
  names()

nms <- colnames(sub_macro)
to_remove <- nms[!(nms %in% to_keep)]

train_macro %<>% select(-one_of(to_remove))
test_macro %<>% select(-one_of(to_remove))

save(
  id_train, train_macro, id_test, test_macro, 
  file = "output/select_columns.RData"
)