############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities that helps with data exploration

############################################################
##                        METHOD                          ##
############################################################

# calculate the mode
mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  result <- NULL
  result$freq <- max(tab)
  result$mode <- ux[tab == max(tab)]
  return(result)
}

# very similar to str function but returns data as a dataframe
str.table <- function(dataset, preview.limit = 10) {
  return(data.frame(
    name = names(dataset),
    type = as.vector(sapply(dataset, class)),
    preview = as.vector(sapply(dataset, function(col) {
      return(paste(paste(col[1:preview.limit], collapse = ", "), "...", sep =
                     " "))
    }))
  ))
}

# a general function to give the most basic statistics of a dataset
report.table <- function(dataset) {
  return(data.frame(
    name = c(
      "Number of instances",
      "Number of features",
      "Number of continuous features",
      "Number of binary features",
      "Number of nominal features",
      "Number of missing values",
      "Percentage of missing values",
      "Maximum Entropy for a feature"
    ),
    value = prettyNum(
      c(
        nrow(dataset),
        ncol(dataset),
        ncol(data.frame(dataset[, sapply(dataset, is.numeric)])),
        ncol(data.frame(dataset[, sapply(dataset, is.logical)])),
        ncol(data.frame(dataset[, sapply(dataset, is.character)])) + ncol(data.frame(dataset[, sapply(dataset, is.factor)])),
        sum(is.na(dataset)),
        sum(is.na(dataset)) / (nrow(dataset) * ncol(dataset)) * 100,
        entropy(rep(1, nrow(dataset)), unit = c("log2"))
      )
    )
  ))
}

# create summary table for numeric data only
summarize <- function(dataset, na.rm = TRUE) {
  preview.limit <- 5
  result <- NULL
  result$categorical <- NULL
  result$continuous <- NULL
  for (name in colnames(dataset)) {
    x <- dataset[, name]
    if (is.character(x) || is.factor(x) || is.logical(x)) {
      result$categorical <- rbind(result$categorical, prettyNum(
        data.frame(
          name = name,
          type = typeof(x),
          level = length(unique(x)),
          value = paste(
            paste(as.data.frame(table(as.factor(
              x
            )))$Var1[1:ifelse(preview.limit < length(unique(x)),
                              preview.limit,
                              length(unique(x)))], collapse = ", "),
            ifelse(preview.limit < length(unique(x)), " ...", ""),
            sep = ""
          ),
          freq = paste(
            paste(as.data.frame(table(as.factor(
              x
            )))$Freq[1:ifelse(preview.limit < length(unique(x)),
                              preview.limit,
                              length(unique(x)))], collapse = ", "),
            ifelse(preview.limit < length(unique(x)), " ...", ""),
            sep = ""
          ),
          n = length(x),
          mode =  paste(paste(mode(x)$mode[1:ifelse(preview.limit < length(mode(x)$mode),
                                          preview.limit,
                                          length(mode(x)$mode))], collapse = ", "),
              ifelse(preview.limit < length(mode(x)$mode), " ...", ""),
              sep = ""
          ), 
          highest_freq = mode(x)$freq,
          missing = sum(is.na(x)),
          missing_percent = sum(is.na(x)) / length(x) * 100,
          entropy = entropy(as.data.frame(table(factor(
            x
          )))$Freq, unit = c("log2"))
        )
      ))
    } else if (is.numeric(x) || is.integer(x) || is.double(x)) {
      result$continuous <- rbind(result$continuous, prettyNum(
        data.frame(
          name = name,
          type = typeof(x),
          mean = mean(x, na.rm = na.rm),
          sd = sd(x, na.rm = na.rm),
          median = median(x, na.rm = na.rm),
          lquartile = quantile(x, .25, na.rm = na.rm, names = FALSE),
          uquartile = quantile(x, .75, na.rm = na.rm, names = FALSE),
          iqr = IQR(x, na.rm = na.rm),
          min = min(x, na.rm = na.rm),
          max = max(x, na.rm = na.rm),
          n = length(x),
          missing = sum(is.na(x)),
          missing_percent = sum(is.na(x)) / length(x) * 100,
          entropy = entropy(as.data.frame(table(factor(
            x
          )))$Freq, unit = c("log2"))
        )
      ))
    }
  }
  result$categorical <- as.data.frame(result$categorical)
  result$continuous <- as.data.frame(result$continuous)
  return(result)
}

# create a table of correlation values and allow sorting
compute.correlation.pair_wise <-
  function(data_set,
           sort = FALSE,
           decreasing = TRUE) {
    i <- 1
    x <- c()
    y <- c()
    corr <- c()
    p <- c()
    while (i <= length(colnames(data_set))) {
      j <- i + 1
      while (j <= length(colnames(data_set))) {
        x <- append(x, colnames(data_set)[i])
        y <- append(y, colnames(data_set)[j])
        corr <-
          append(corr,
                 cor(data_set[, i], data_set[, j], use = "pairwise.complete.obs"))
        p <-
          append(p, cor.test(data_set[, i], data_set[, j])$p.value)
        j <- j + 1
      }
      i <- i + 1
    }
    corr_result <- data.frame(x = x,
                              y = y,
                              cor = corr,
                              p = p)
    if (sort) {
      result <-
        corr_result[order(abs(corr_result$cor), decreasing = decreasing),]
    } else {
      result <- corr_result
    }
  }
