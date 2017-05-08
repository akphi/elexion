############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities that helps with data transformation

############################################################
##                        METHOD                          ##
############################################################

# turn columns of dataset with name in list to NULL
nullify <- function(dataset, list) {
  for (name in list) {
    dataset[, name] <- NULL
  }
  return(dataset)
}

# normalize: bring every instance values to range [0-1], beware of outliers
normalize <- function(input, na.rm = TRUE) {
  input_min <- min(input, na.rm = na.rm)
  input_max <- max(input, na.rm = na.rm)
  result <- (input - input_min) / (input_max - input_min)
}

# standardize: bring the mean to 0
standardize <- function(input, na.rm = TRUE) {
  input_mean <- mean(input, na.rm = na.rm)
  input_sd <- sd(input, na.rm = na.rm)
  result <- (input - input_mean) / input_sd
}

# transform each numerical feature in dataset using provided function
transform.numeric_data <-
  function(dataset,
           FUN = function(x) {
             return(x)
           },
           na.rm = TRUE) {
    for (name in names(dataset)) {
      if (is.numeric(dataset[, name])) {
        dataset[, name] <- FUN(dataset[, name], na.rm)
      } else {
        dataset[, name] <- dataset[, name]
      }
    }
    return(dataset)
  }

# turn all logical features into factors
all.logical_to_factor <- function(dataset) {
  dataset[, sapply(dataset, is.logical)] <- lapply(dataset[sapply(dataset, is.logical)], as.factor)
  return(dataset)
}

# sampling with replacement ONLY for lower-size class in over-sampling
sampling_with_flexible_replacement <-
  function(data,
           target,
           type = NULL,
           n = NULL,
           shuffle = FALSE) {
    size <- n
    if (type == "over") {
      size <- max(dplyr::count_(data, target)$n)
    } else if (type == "under") {
      size <- min(dplyr::count_(data, target)$n)
    } else if (type == "none") {
      return(data)
    } else {
      if (is.null(n)) {
        return(data)
      } else {
        size <- n
      }
    }
    # debuggind purpose
    # data %>% group_by_(target) %>% do(print(paste(size, dplyr::count(.)$n, ifelse(dplyr::count(.)$n < size, TRUE, FALSE), sep =" ")))
    result <- as.data.frame(data %>%
                              group_by_(target) %>% do(sample_n(
                                ., size, replace = ifelse(dplyr::count(.)$n < size, TRUE, FALSE)
                              )))
    if (shuffle) {
      result <- result[sample(nrow(result)),]
    }
    return(result)
  }

# convert all columns of a dataframe into numeric
numerize_dataframe <- function(dataset) {
  # convert all nominal features to binary
  for (name in names(dataset)) {
    if (is.factor(dataset[, name]) || is.character(dataset[, name])) {
      dataset[, name] <- as.factor(dataset[, name])
      binarized <- cbind(with(dataset, model.matrix(as.formula(paste("~", name, "+0", sep = "")))))
      dataset[, name] <- NULL
      dataset <- cbind(dataset, binarized)
    }
  }
  # convert all binary features to numeric
  for (name in names(dataset)) {
    if (is.logical(dataset[, name])) {
      dataset[, name] <- as.numeric(dataset[, name])
    }
  }
  return(dataset)
}

# perform data clamp transformation based on meta data info of each descriptive feature
clamp_bound <- function(input, metadata) {
  dataset <- input
  for (name in names(dataset)) {
      if (is.numeric(dataset[, name])) {
        meta <- metadata[metadata$name == name, ]
        if (!is.na(meta$lbound)) {
          if(length(dataset[, name][!is.na(dataset[, name]) & dataset[, name] < meta$lbound]) > 0) { 
            log.write(
              paste(
                "Lower Clamp occurs to ",
                name,
                ", where instance at index (",
                paste(which(!is.na(dataset[, name]) & dataset[, name] < meta$lbound), collapse = ", "),
                ") with value (",
                paste(dataset[, name][!is.na(dataset[, name]) & dataset[, name] < meta$lbound], collapse = ", "),
                ") falling below the lower bound at ",
                meta$lbound,
                sep = ""
              )
            )
          }
          dataset[, name][!is.na(dataset[, name]) & dataset[, name] < meta$lbound] <- meta$lbound
        }
        if (!is.na(meta$ubound)) {
          if(length(dataset[, name][!is.na(dataset[, name]) & dataset[, name] > meta$ubound]) > 0) { 
            log.write(
              paste(
                "Upper Clamp occurs to ",
                name,
                ", where instance at index (",
                paste(which(!is.na(dataset[, name]) & dataset[, name] > meta$ubound), collapse = ", "),
                ") with value (",
                paste(dataset[, name][!is.na(dataset[, name]) & dataset[, name] > meta$ubound], collapse = ", "),
                ") exceeding the upper bound at ",
                meta$ubound,
                sep = ""
              )
            )
          }
          dataset[, name][!is.na(dataset[, name]) & dataset[, name] > meta$ubound] <- meta$ubound
        }
      } else {
        dataset[, name] <- dataset[, name]
      }
    }
  return(dataset)
}