#' Add dummy variables
#'
#' @param dts 
#' @param as_matrix 
#'
#' @return
#' @export
#'
#' @examples
add_dummies <- function(dts, as_matrix=TRUE) {
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
    
    if (as_matrix)
      as.matrix(out)
    else
      out
  }
}


######################################################################

#' Clean-up a dataset
#'
#' @param dataset 
#' @param min_time 
#' @param create_subsets 
#' @param remove_sub_area 
#' @param add_dummies 
#'
#' @return
#' @export
#'
#' @examples
clean_dataset <- function(dataset, 
                          min_time="2011-08-20",
                          create_subsets=FALSE,
                          remove_sub_area=TRUE,
                          add_dummies=FALSE) {
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
  
  ### Remove sub_area due to the large number of levels.
  if (remove_sub_area)
    dataset %<>% select(-sub_area)
  
  if (add_dummies)
    add_dummies(dataset)
  else
    dataset
}



#' Create time slices for time moving CV
#'
#' @param N 
#' @param n_fold 
#' @param width 
#'
#' @return
#' @export
#'
#' @examples
create_slices <- function(N=30471, n_fold=5, width=3000) {
  idx_train <- lapply(0:(n_fold - 1), function(k) {
    m <- k * 2000
    seq(m + 1, m + N)
  })
  
  idx_val <- lapply(0:(n_fold - 1), function(k) {
    seq(N + k * 2000 + 1, N + (k + 1) * 2000)
  })
}


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
  sub_name <- paste0("output/submissions/", 
                     gsub("[ :]", "_", date()), ".csv")
  readr::write_csv(submission, path = sub_name)
}