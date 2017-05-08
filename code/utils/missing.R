############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities that helps to handle missing data

############################################################
##                        METHOD                          ##
############################################################

# compare the result of the same county and take the more recent result
# if the most recent result is missing, fill in with the result from the past
# plus the average difference of the feature between 2 results of all counties
# of the same state as the count ywith missing data
compute.prediction_by_diff.by_state <-
  function(template, main, supp, attribute) {
    data_temp <-
      merge(
        x = template,
        y = data.frame(fips = main[, "fips"], main = main[, attribute]),
        all.x = TRUE,
        by = "fips"
      )
    data_temp <-
      merge(
        x = data_temp,
        y = data.frame(fips = supp[, "fips"], supp = supp[, attribute]),
        all.x = TRUE,
        by = "fips"
      )
    data_temp$diff <- data_temp$main - data_temp$supp
    # the NA difference are considered 0
    data_temp[is.na(data_temp$diff),]$diff <- 0
    data_temp <-
      merge(
        x = data_temp,
        y = aggregate(
          x = data_temp$diff,
          by = list(data_temp$state),
          FUN = "mean"
        ),
        all.x = TRUE,
        by.x = "state",
        by.y = "Group.1"
      )
    colnames(data_temp)[names(data_temp) == "x"] <- "diff_by_state"
    data_temp$res <- data_temp$main
    data_temp[is.na(data_temp$main),]$res <-
      data_temp[is.na(data_temp$main),]$supp + data_temp[is.na(data_temp$main),]$diff_by_state
    result <- data.frame(fips = data_temp$fips, res = data_temp$res)
    colnames(result)[names(result) == "res"] <- attribute
    return(result)
  }

# the most important dataset goes first
# all the dataframe must contain the attribute needed
combine.prediction_by_diff <-
  function(...,
           template = template,
           attrList = NULL) {
    if (missing(attrList)) {
      return(NULL)
    }
    data_list <- list(...)
    result <- template
    for (attr in attrList) {
      temp <- data.frame(data_list[length(data_list)])
      i <- length(data_list) - 1
      while (i > 0) {
        temp <-
          compute.prediction_by_diff.by_state(template, data.frame(data_list[i]), temp, attr)
        i <- i - 1
      }
      result <-
        merge(
          x = result,
          y = temp[, c("fips", attr)],
          all.x = TRUE,
          by = "fips"
        )
    }
    return(result)
  }

# compute the missing value by averaging values from certain source
# that share aspect with the instance (same county, same state)
compute.average.from_source <-
  function(target,
           source,
           by.target,
           by.source,
           names) {
    for (name in names) {
      # aggregate from source
      temp_aggregate <-
        aggregate(
          x = source[, name],
          by = list(source[, by.source]),
          FUN = "mean",
          na.rm = TRUE,
          na.action = NULL
        )
      target <-
        merge(
          x = target,
          y = temp_aggregate,
          all.x = TRUE,
          by.x = by.target,
          by.y = "Group.1"
        )
      # the na.rm options above might cause some mean to be NaN so we have to replace all NaN by NA
      target[is.na(target[, name]),][, name] <-
        target[is.na(target[, name]),][, "x"]
      target[, name][is.nan(target[, name])] <- NA
      target$x <- NULL
    }
    return(target)
  }