add_dummies <- function(dts) {
  require(magrittr)
  require(tidyverse)
  require(caret)
  
  ### Timestamps need to be dealt with separately
  if ("timestamp" %in% names(dts)) {
    tst <- dts[["timestamp"]]
    dts[["timestamp"]] <- NULL
  }
  
  ### Separate numeric and character variables
  is_char <- dts %>% map_lgl(is.character)
  is_numeric <- dts %>% map_lgl(is.numeric)
  if (sum(is_char) + sum(is_numeric) != ncol(dts))
    stop("Some variables have been missed. Please check.")

  ### If none of the variables is character, return the dataset as
  ### a matrix
  if (!any(is_char)) {
    return(as.matrix(dts))
  } else {
    
    ### Separate numeric and non-numeric variables
    num_dts <- dts[is_numeric]
    chr_dts <- dts[is_char]
    
    ### Accessory functions
    n_levels <- function(x) {nlevels(as.factor(x))}
    is_binary <- function(x) {n_levels(x) == 2}
    
    ### Separate binary and multi-category variables
    idx_binary <- chr_dts %>% map_lgl(is_binary)
    binary <- chr_dts[idx_binary]
    if (any(idx_binary))
      multicat <- chr_dts[!idx_binary]
    
    ### Create dummy variables for the binary predictors (Full Rank)
    db <- dummyVars(~ ., data = binary, fullRank = TRUE)
    dummy_binary <- predict(db, binary)
    
    ### Create dummy variables for the multi-category predictors
    ### (not Full Rank)
    dm <- dummyVars(~ ., data = multicat)
    dummy_multicat <- predict(dm, multicat)
    
    ### Put together the dts
    out <- cbind(num_dts, dummy_binary, dummy_multicat)
    as.matrix(out)
  }
}


######################################################################

clean_dataset <- function(dataset, 
                          min_time="2011-08-20",
                          remove_sub_area=TRUE) {
  require(magrittr)
  require(tidyverse)

  ### Compute the number of weeks from the first date in the dataseting
  ### set.
  starting_time <- as.Date(min_time)
  dataset$week <- as.integer(1 + trunc(
    (dataset$timestamp - starting_time) / 7
  ))
  
  ### Time variables
  dataset %<>% mutate(
    year = lubridate::year(timestamp),
    month = lubridate::month(timestamp)
  )
  
  ### Demographic variables
  dataset %<>% mutate(
    popul_dens_raion = raion_popul / area_m, # Population density
    full_dens = full_all / area_m,
    young_dens = young_all / area_m,
    work_dens = work_all / area_m,
    elder_dens = ekder_all / area_m
  )
  
  # Materials as from Mrs. Berdnikova discussion
  materials <- c(
    "1" = "panel", 
    "2" = "brick", 
    "3" = "wood", 
    "4" = "mass_concrete", 
    "5" = "breezeblock", 
    "6" = "mass_concrete_brick"
  )
  
  ### Building/flat specific variables. 
  dataset %<>% mutate(
    max_floor = as.integer(max_floor),
    build_year = as.integer(build_year),
    num_room = as.integer(num_room),
    kitch_sq = as.integer(kitch_sq),
    material = materials[material]
  )
  
  ### Fix entry mistakes
  dataset %<>% mutate(
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
    floor = ifelse(floor > max_floor, NA, floor),
    rel_floor = floor / max_floor
  )
  
  ### Start removing some variables: Here we are only considering
  ### Presence/absence of a feature
  dataset_essential <- dataset %>%
    select(
      timestamp:sub_area, rel_floor,
      popul_dens_raion,
      green_zone_part:detention_facility_raion,
      ecology,
      price_doc:elder_dens
    )
  
  ### Remove sub_area due to the large number of levels.
  if (remove_sub_area)
    dataset_essential %<>% select(-sub_area)
  
  essential_matrix <- add_dummies(dataset_essential)
  rm(dataset_essential)
  
  ### Consider the variables that have `count` in the name
  dataset_count <- dataset %>% select(
    raion_build_count_with_material_info:build_count_after_1995
  )
  
  ### Remove these variables based on findCorrelation
  remove_count <- c(
    "raion_build_count_with_builddate_info", 
    "build_count_1921-1945", 
    "build_count_wood"
  )
  
  dataset_count %<>% select(-one_of(remove_count))
  
  count_matrix <- add_dummies(dataset_count)
  rm(dataset_count)
  
  ### Consider the variables that have `_km` in the name
  dataset_km <- dataset %>% select(contains("_km"))
  
  ### Remove these columns from dataset_km based on 
  ### caret::findCorrelation
  remove_km <- c("basketball_km", "radiation_km", "big_church_km",
    "metro_km_walk", "ttk_km", "metro_km_avto", "zd_vokzaly_avto_km", 
    "stadium_km", "sadovoe_km", "swim_pool_km", "bulvar_ring_km",
    "school_km", "preschool_km", "museum_km", "railroad_station_walk_km")
  
  dataset_km %<>% select(-one_of(remove_km))
  
  km_matrix <- add_dummies(dataset_km)
  rm(dataset_km)
  
  ### Variables counting objects within a certain radius
  dataset_radius <- dataset %>%
    select(green_part_500:market_count_5000)
  
  remove_radius <- scan(
    file = "data/remove_radius.txt", what = "", sep = ","
  )
  
  dataset_radius %<>% select(-one_of(remove_radius))
  
  radius_matrix <- add_dummies(dataset_radius)
  rm(dataset_radius)
  
  ### I don't understand why radiation_km is positively correlated with
  ### nuclear_reactor_km
  ### qplot(x = radiation_km, y = nuclear_reactor_km,
  ### data = dataset, alpha = I(0.05))
  
  ### Add dummy variables. Full Rank for binary vars, otherwise for
  ### multi-level ones.
  
  return(
    list(
      essential = essential_matrix,
      count = count_matrix,
      km = km_matrix,
      radius = radius_matrix
    )
  )
  
}