############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities that support feature selection

############################################################
##                        METHOD                          ##
############################################################

# finds attribute subset using correlation and entropy measures for continous and discrete data
exhaustive.cfs <- function(dataset, target) {
  colnames(dataset)[names(dataset) == target] <- "cfs_target"
  result <- NULL
  iteration <- NULL
  counter <- 1
  # to avoid the target
  while (length(result) != (ncol(dataset) - 1)) {
    cfs_result <-
      cfs(cfs_target ~ ., dataset[,!(names(dataset) %in% result)])
    result <- append(result, cfs_result)
    iteration <- append(iteration, rep(counter, length(cfs_result)))
    counter <- counter + 1
  }
  return(data.frame(name = result, iteration = iteration))
}

# turn a formula to string
formula.to_text <- function(formula) {
  result <- as.character(formula)
  result <- gsub("\n", "", result)
  result <- gsub(" ", "", result)
  result <- gsub("\\+", " + ", result)
  result <- gsub("~", " ~ ", result)
  return(result)
}

# count number of variables seen in the given list of formula
formula.attribute_counter <- function(attributes, flist, target) {
  attributes <- getrov(attributes, c(target))
  attributes <- data.frame(name = attributes)
  result <- NULL
  formula <- unlist(flist)
  for (formulae in formula) {
    result <- append(result, all.vars(formulae))
  }
  result <- as.data.frame(table(factor(result)))
  colnames(result) <- c("name", "count")
  result <- merge(x = attributes,
                  y = result,
                  all.x = TRUE,
                  by = "name")
  result <- result[order(abs(result$count), decreasing = TRUE),]
  return(result)
}

# calculate different statistics based on confusion matrix
# we use confusionMatrix function from caret package to help 
# us construct the matrix effectively and to avoid all sorts
# of trouble with mismatching levels. nevertheless, caret
# will still warn us if level mismatch occur
# e.g. In confusionMatrix.default(... Levels are not in the 
# same order for reference and data. Refactoring data to match
fs.generate.evaluation <-
  function(prediction_list,
           actual_list,
           target_type,
           model,
           verbose) {
    result <- NULL
    if (target_type == "numeric") {
      # for numeric target feature
      result$mse <- mean((prediction_list - actual_list)^2, na.rm = TRUE)
      result$rmse <- sqrt(result$mse)
      result$mae <- mean(abs(prediction_list - actual_list), na.rm = TRUE)
      result$rae <- sum(abs(prediction_list - actual_list))/sum(abs(mean(actual_list, na.rm = TRUE) - actual_list))
      result$rrse <- sqrt(sum((prediction_list - actual_list)^2)/sum((mean(actual_list, na.rm = TRUE) - actual_list)^2))
    } else if (target_type == "nominal") {
      # for multiple-class instance
      matrix <- NULL
      tryCatch(matrix <- confusionMatrix(prediction_list, actual_list, mode = "everything")$table,
             warning = function(w) {
               # Handle warnings caused by linear model
              if(w == "Levels are not in the same order for reference and data. Refactoring data to match." && model == "linear") {
                if(verbose) {
                  warning(paste("KNOWN ISSUE: caused by linear model sometimes give out the whole prediction set as TRUE or FALSE:", w, sep = "\n"))
                }
              } else {
                warning(w)
              }
              suppressWarnings(matrix <<- confusionMatrix(prediction_list, actual_list, mode = "everything")$table)
            }
      )
      result$matrix <- matrix
      result$accuracy <- sum(diag(matrix)) / sum(matrix)
      result$misclassification <- 1 - result$accuracy
      result$recall <- nan_to_zero(diag(matrix) / colSums(matrix))
      result$precision <- nan_to_zero(diag(matrix) / rowSums(matrix))
      result$prevalence <- colSums(matrix) / sum(matrix)
      result$detection_prevalence <- rowSums(matrix) / sum(matrix)
      result$kappa <-
        (result$accuracy - sum(result$prevalence * result$detection_prevalence)) / (1 - sum(result$prevalence * result$detection_prevalence))
      result$f1 <-
        2 * result$precision * result$recall / (result$precision + result$recall)
    } else if (target_type == "logical") {
      # for 2-class problem, we use the caret library just for the sake of it
      matrix <- confusionMatrix(prediction_list, actual_list, mode = "everything")
      result$matrix <- matrix$table
      result$positive_class <- matrix$positive
      result$accuracy <- matrix$overall["Accuracy"]
      result$misclassification <- 1 - result$accuracy
      result$recall <- matrix$byClass["Recall"]
      result$specificity <- matrix$byClass["Specificity"]
      result$precision <- matrix$byClass["Precision"]
      result$prevalence <- matrix$byClass["Prevalence"]
      result$detection_rate <- matrix$byClass["Detection Rate"]
      result$detection_prevalence <-
        matrix$byClass["Detection Prevalence"]
      result$kappa <- matrix$overall["Kappa"]
      result$f1 <- matrix$byClass["F1"]
    } else {
      warning("unsupported target type")
    }
    # print(result)
    return(result)
  }

# generate prediction based on model constructed using training set and test against the test set
fs.generate.prediction <-
  function(train, test, subset, target, model, verbose) {

    # CART
    if (model == "tree") {
      tree <- rpart(as.simple.formula(subset, target),
                    method = "class",
                    data = train)
      return(predict(tree, test, type = "class"))
    }

    # Linear Regression
    # this sometimes creates models that predicts TRUE or FALSE only
    # due to the skewed distribution in class values of the target
    # and confusionMatrix in fs.generate.evaluation() will complain
    if (model == "linear") {
      train[, target] <- as.numeric(as.logical(train[, target]))
      linear <- lm(as.simple.formula(subset, target), data = train)
      pred <- predict(linear, test)
      pred <- as.factor(as.logical(vround(pred, c(0,1))))
      return(pred)
    }

    warning("model is not found")
    return(NULL)
  }

# a wrapper method to return a evaluator using different model and metric
fs.generate.validator <- function(dataset,
                               target,
                               target_type,
                               model,
                               metric,
                               metric.class = NULL,
                               fold,
                               sampling,
                               verbose) {
  template = function(subset) {
    splits <- createFolds(dataset[, target], k = ifelse(is.null(fold), FEATURE_RANKING_SEARCH_CROSS_VALIDATION_FOLD, fold), list = TRUE)
    results = alply(splits, 1, function(i) {
      train <- dataset[-i,]
      train <- sampling_with_flexible_replacement(train, target, type = sampling, shuffle = TRUE)
      test <- dataset[i,]
      pred_result <-
        fs.generate.prediction(train, test, subset, target, model, verbose)
      result <-
        fs.generate.evaluation(pred_result, test[, target], target_type, model, verbose)
      if (is.null(metric.class)) {
        # print(result)
        result <- unlist(result[metric])
      } else {
        result <- unlist(result[metric])[[paste(metric, ".", metric.class, sep = "")]]
      }
      # print(subset)
      # print(result)
      return(result)
    },
    .parallel = TRUE)
    # print(mean(unlist(results)))
    return(mean(unlist(results)))
  }
  return(template)
}

# acuracy evaluator using different forms of search
fs.feature_select <-
  function(dataset,
           target,
           evaluator = NULL,
           eval.opts = NULL,
           method = c("best-first", "forward", "backward", "hill-climbing"),
           iteration = FEATURE_RANKING_SEARCH_ITERATION,
           sampling = "none",
           verbose = TRUE) {
    result <- NULL
    attribute_names <-
      names(dataset[, names(dataset) %notin% c(target)])
    
    if (is.null(evaluator)) {
      if (is.null(eval.opts)) {
        warning("no evaluator found")
        return(NULL)
      }
      evaluator <- fs.generate.validator(
        dataset,
        target,
        target_type = eval.opts$target_type,
        model = eval.opts$model,
        metric = eval.opts$metric,
        metric.class = eval.opts$metric.class,
        fold = eval.opts$fold,
        sampling = sampling,
        verbose = verbose
      )
    }
    
    # BEST-FIRST
    # note: the algorithm is similar to forward.search besides the fact that is chooses
    # the best node from all already evaluated ones and evaluates it
    if ("best-first" %in% method) {
      best_first_search <- NULL
      for (j in 1:iteration) {
        best_first_search <- append(best_first_search,
                                    as.simple.formula(best.first.search(attribute_names, evaluator), target))
      }
      result$best_first_search <- best_first_search
    }
    
    # FORWARD
    # note: this algorithm implements greedy search. At first, the algorithms expand starting node,
    # evaluate its children and choose the best one which becomes a new starting node.
    if ("forward" %in% method) {
      forward_search <- NULL
      for (j in 1:iteration) {
        forward_search <- append(forward_search,
                                 as.simple.formula(forward.search(attribute_names, evaluator), target))
      }
      result$forward_search <- forward_search
    }
    
    # BACKWARD
    # note: this algorithm implements greedy search. At first, the algorithms expand starting node,
    # evaluate its children and choose the best one which becomes a new starting node.
    if ("backward" %in% method) {
      backward_search <- NULL
      for (j in 1:iteration) {
        backward_search <- append(backward_search,
                                  as.simple.formula(backward.search(attribute_names, evaluator), target))
      }
      result$backward_search <- backward_search
    }
    
    # HILL-CLIMBING
    # note: this algorithm starts with a random attribute set. Then it evaluates
    # all its neighbours and chooses the best one. It might be susceptible to local maximum.
    if ("hill-climbing" %in% method) {
      hill_climbing <- NULL
      for (j in 1:iteration) {
        hill_climbing <- append(hill_climbing,
                                as.simple.formula(
                                  hill.climbing.search(attribute_names, evaluator),
                                  target
                                ))
      }
      result$hill_climbing <- hill_climbing
    }
    
    # EXHAUSTIVE
    # note: does everything.
    if ("exhaustive" %in% method) {
      exhaustive <- NULL
      for (j in 1:iteration) {
        exhaustive <- append(exhaustive,
                             as.simple.formula(exhaustive.search(attribute_names, evaluator), target))
      }
      result$exhaustive <- exhaustive
    }
    
    formula <- result
    for (i in 1:length(formula)) {
      formula[i] <- as.vector(lapply(formula[i], formula.to_text))
    }
    formula <- data.frame(formula)
    
    result <- list(
      "counts" = formula.attribute_counter(names(dataset), result, target),
      "formula" = formula,
      "raw" = result
    )
    return(result)
  }

# construct linear model and sort the attribtue name based on the absolute of its coefficient
# if remove.smallest.first is TRUE, drop the smallest first then repeat the procedure for the rest
# othewise, drop the biggest and repeat the procedure
# the most significant ones are always on top
exhaustive.linear_model.by_coefficient <-
  function(dataset, target, remove.smallest.first = TRUE) {
    colnames(dataset)[names(dataset) == target] <- "lm_target"
    result <- NULL
    # ncol() returns NULL when the dataframe turns into a vector
    while (!is.null(ncol(dataset))) {
      model <- lm(lm_target ~ ., data = dataset)
      rank <- data.frame(coefficient = abs(model$coefficients))
      rank$name <- rownames(rank)
      rank <- rank[rank$name != "(Intercept)",]
      if (remove.smallest.first == TRUE) {
        result <-
          append(rank$name[rank$coefficient == min(rank$coefficient)], result)
      } else {
        result <-
          append(result, rank$name[rank$coefficient == max(rank$coefficient)])
      }
      dataset <- getrod(dataset, result)
    }
    return(result)
  }

# based on the relative rank of the feature, assign a score for them
# the top one has the highest and this decrements down the list
feature_rank.by_importance <- function(dataset, highest) {
  for (i in 2:ncol(dataset)) {
    seed <- highest
    dataset <-
      dataset[order(abs(dataset[, i]), decreasing = TRUE),]
    dataset[!is.na(dataset[, i]), i][1] <- seed
    na.number <- !is.na(dataset[, i])
    for (j in 2:nrow(dataset[!is.na(dataset[, i]), ])) {
      if (dataset[!is.na(dataset[, i]), i][j] < dataset[!is.na(dataset[, i]), i][j -
                                                                                 1]) {
        seed <- seed - 1
      }
      dataset[!is.na(dataset[, i]), i][j] <- seed
    }
  }
  return(dataset)
}

# create a score table with weight assign for each metric
feature_rank.compile <- function(template, data_list, weights) {
  for (i in 1:length(data_list)) {
    for (j in 1:ncol(data_list[[i]])) {
      if (is.numeric(data_list[[i]][, j])) {
        data_list[[i]][, j] <- data_list[[i]][, j] * weights[i]
      }
    }
  }
  for (i in 1:length(data_list)) {
    template <-
      merge(
        x = template,
        y = data_list[[i]],
        by = "name",
        all = TRUE
      )
  }
  template$score <- rowSums(template[, -1], na.rm = FALSE)
  template <-
    template[order(template[, "score"], decreasing = TRUE), ]
  return(template)
}