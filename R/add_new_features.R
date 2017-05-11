library(magrittr)
library(tidyverse)

load("output/clean_columns.RData")

prices_by_area_year <- train_macro %>%
  group_by(sub_area, year) %>%
  summarise(
    mean_price_by_area = mean(price_doc, na.rm = TRUE),
    median_price_by_area = median(price_doc, na.rm = TRUE)
  )

prices_per_sqm_year <- train_macro %>%
  mutate(price_per_sqm = price_doc / full_sq) %>%
  group_by(sub_area, year) %>%
  summarise(
    mean_price_per_sqm = mean(price_per_sqm, na.rm = TRUE),
    median_price_per_sqm = median(price_per_sqm, na.rm = TRUE)
  )

### IMPORTANT
### We don't have these values in the test set, so we should try to
### predict them.
prices <- inner_join(prices_by_area_year, prices_per_sqm_year)
train_macro %<>% inner_join(prices, by = c('sub_area', 'year'))

save(
  id_train, train_macro, id_test, test_macro, 
  file = "output/add_new_features.RData"
)
