library(magrittr)
library(tidyverse)

train <- read_csv("data/train.csv.gz")

### Compute the number of weeks from the first date in the training
### set.
starting_time <- min(train$timestamp)
train$week <- as.integer(1 + trunc(
  (train$timestamp - starting_time) / 7
))

### Time variables
train %<>% mutate(
  year = lubridate::year(timestamp),
  month = lubridate::month(timestamp)
)

### Demographic variables
train %<>% mutate(
  popul_dens_raion = raion_popul / area_m, # Population density
  full_dens = full_all / area_m,
  young_dens = young_all / area_m,
  work_dens = work_all / area_m
)

### Building/flat specific variables. 
train %<>% mutate(
  max_floor = as.integer(max_floor),
  build_year = as.integer(build_year),
  num_room = as.integer(num_room),
  kitch_sq = as.integer(kitch_sq)
)

### Fix entry mistakes
train %<>% mutate(
  build_year = ifelse(build_year < 500, NA, build_year),
  build_year = ifelse(build_year > 3000, 2007, build_year),
  state = ifelse(state == "33", NA, state),
  kitch_sq = ifelse(kitch_sq > 150, NA, kitch_sq),
  life_sq = ifelse(life_sq == 0, NA, life_sq),
  life_sq = ifelse(life_sq > 2000, NA, life_sq),
  full_sq = ifelse(full_sq == 0, NA, full_sq),
  full_sq = ifelse(full_sq > 2000, NA, full_sq),
  life_sq = ifelse(life_sq > full_sq, NA, life_sq),
  num_room = ifelse(num_room == 0, NA, num_room),
  floor = ifelse(floor > max_floor, NA, floor)
)

# Take a look at the number of transactions by sub_area by year
# transactions_per_year <- train %>% 
#   group_by(year, sub_area) %>% 
#   summarise(n_transactions = n())
# 
# ggplot(
#   data = tmp, 
#   aes(x = reorder(sub_area, n_transactions), 
#     y = n_transactions)) + 
#   geom_bar(stat = "identity") + 
#   facet_wrap(~ year) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
#   )

# ### Drop the following two columns, as they have very low variance,
# ### and they are difficult to interpret
# train %<>% select(
#   -c(`0_6_all`:raion_build_count_with_builddate_info,
#     modern_education_share, old_education_build_share
#   ), -starts_with('cafe')
# )
# 
# ### BINARIZING THE CATEGORICAL VARIABLES WITH ONLY TWO LEVELS
# 
# tmp <- train[sapply(train, is.character)]
# ncat <- sapply(tmp, function(x) length(unique(x)))
# binary <- names(ncat)[ncat == 2]
# 
# train %<>% mutate(
#   product_type = as.integer(
#     ifelse(product_type == "Investment", 1, 0)
#   ),
#   culture_objects_top_25 = as.integer(
#     culture_objects_top_25 == 'yes'
#   ),
#   water_1line = as.integer(water_1line == 'yes'),
#   railroad_1line = as.integer(railroad_1line == 'yes'),
#   big_road1_1line = as.integer(big_road1_1line == 'yes'),
#   ecology = ifelse(ecology == "no data", NA, ecology)
# )
