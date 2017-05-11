library(magrittr)
library(tidyverse)

load("output/merge_with_macro.RData")

### Compute the number of weeks from the first date in the training
### set.
starting_time <- min(train_macro$timestamp)
train_macro$week <- as.integer(1 + trunc(
  (train_macro$timestamp - starting_time) / 7
))

test_macro$week <- as.integer(1 + trunc(
  (test_macro$timestamp - starting_time) / 7
))

### We can now drop the timestamp
### train_macro$timestamp <- test_macro$timestamp <- NULL

### FIXING THE COLUMNS OF THE TRAINING SET
### The columns from max_floor to kitch_sq should be numeric
### NOTE build_year and kitch_sq contain entry mistakees
train_macro %<>% mutate(
  max_floor = as.integer(max_floor),
  build_year = as.integer(build_year),
  num_room = as.integer(num_room),
  kitch_sq = as.integer(kitch_sq)
)

train_macro %<>% mutate(
  build_year = ifelse(build_year < 100, NA, build_year),
  build_year = ifelse(build_year > 3000, 2007, build_year),
  state = ifelse(state == "33", NA, state),
  kitch_sq = ifelse(kitch_sq > 150, NA, kitch_sq),
  life_sq = ifelse(life_sq == 0, NA, life_sq),
  full_sq = ifelse(full_sq == 0, NA, full_sq)
)

### Drop the following two columns, as they have very low variance,
### and they are difficult to interpret
train_macro %<>% select(
  -modern_education_share, -old_education_build_share
)


### FIXING THE COLUMNS OF THE TEST SET
### All the character columns of the test set seem in order
test_macro %<>% mutate(
  build_year = ifelse(build_year < 100, NA, build_year),
  kitch_sq = ifelse(kitch_sq > 150, NA, kitch_sq),
  life_sq = ifelse(life_sq == 0, NA, life_sq),
  full_sq = ifelse(full_sq == 0, NA, full_sq)
)

### BINARIZING THE CATEGORICAL VARIABLES WITH ONLY TWO LEVELS

### TRAINING SET
tmp <- train_macro[sapply(train_macro, is.character)]
ncat <- sapply(tmp, function(x) length(unique(x)))
binary <- names(ncat)[ncat == 2]

train_macro %<>% mutate(
  product_type = as.integer(
    ifelse(product_type == "Investment", 1, 0)
  ),
  culture_objects_top_25 = as.integer(
    culture_objects_top_25 == 'yes'
  ),
  water_1line = as.integer(water_1line == 'yes'),
  railroad_1line = as.integer(railroad_1line == 'yes'),
  big_road1_1line = as.integer(big_road1_1line == 'yes'),
  ecology = ifelse(ecology == "no data", NA, ecology)
)

### Same operation for the test set
tmp <- test_macro[sapply(test_macro, is.character)]
ncat <- sapply(tmp, function(x) length(unique(x)))
binary <- names(ncat)[ncat == 2]

test_macro %<>% mutate(
  product_type = as.integer(
    ifelse(product_type == "Investment", 1, 0)
  ),
  culture_objects_top_25 = as.integer(
    culture_objects_top_25 == 'yes'
  ),
  water_1line = as.integer(water_1line == 'yes'),
  railroad_1line = as.integer(railroad_1line == 'yes'),
  big_road1_1line = as.integer(big_road1_1line == 'yes'),
  ecology = ifelse(ecology == "no data", NA, ecology),
  material = as.character(material),
  state = as.character(state)
)

save(
  id_train, train_macro, id_test, test_macro, 
  file = "output/clean_columns.RData"
)