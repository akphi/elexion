############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities for evaluations: used for tuning model parameters

############################################################
##                        METHOD                          ##
############################################################

# generate data frame of result
tuner.generate.evaluation <-
  function(prediction_list,
           actual_list,
           classifier,
           verbose) {
    # computation
    result <- NULL
    matrix <- NULL
    tryCatch(matrix <- confusionMatrix(prediction_list, actual_list, mode = "everything")$table,
            warning = function(w) {
              # Handle warnings caused by linear model
              if (w$message == "Levels are not in the same order for reference and data. Refactoring data to match.") {
                if (classifier == "lmm" || classifier == "neuralnet") {
                  if (verbose == 2) {
                    warning(paste("KNOWN ISSUE: caused by linear model sometimes give out the whole prediction set as TRUE or FALSE:", w, sep = "\n"))
                  }
                } else {
                  warning(paste(paste(classifier, "with levels:", paste(levels(prediction_list), collapse = ", "), sep = " "), w, sep = "\n"))
                }
              } else {
                warning(w)
              }
              suppressWarnings(matrix <<- confusionMatrix(prediction_list, actual_list, mode = "everything")$table)
            }
      )
    freq <- as.data.frame(matrix)
    accuracy <- sum(diag(matrix)) / sum(matrix)
    misclassification <- 1 - accuracy
    recall <- nan_to_zero(diag(matrix) / colSums(matrix))
    precision <- nan_to_zero(diag(matrix) / rowSums(matrix))
    prevalence <- colSums(matrix) / sum(matrix)
    detection_prevalence <- rowSums(matrix) / sum(matrix)
    f1 <- 2 * precision * recall / (precision + recall)
    kappa <- (accuracy - sum(prevalence * detection_prevalence)) / (1 - sum(prevalence * detection_prevalence))
    # assignment
    result$accuracy <- accuracy
    result$kappa <- kappa
    result$misclassification <- misclassification
    result$recall.TRUE <- recall["TRUE"]
    result$recall.FALSE <- recall["FALSE"]
    result$precision.TRUE <- precision["TRUE"]
    result$precision.FALSE <- precision["FALSE"]
    result$prevalence.TRUE <- prevalence["TRUE"]
    result$prevalence.FALSE <- prevalence["FALSE"]
    result$detection_prevalence.TRUE <- detection_prevalence["TRUE"]
    result$detection_prevalence.FALSE <- detection_prevalence["FALSE"]
    result$f1.TRUE <- f1["TRUE"]
    result$f1.FALSE <- f1["FALSE"]
    result$TP <- freq[freq$Prediction == TRUE & freq$Reference == TRUE, ]$Freq
    result$FP <- freq[freq$Prediction == TRUE & freq$Reference == FALSE, ]$Freq
    result$FN <- freq[freq$Prediction == FALSE & freq$Reference == TRUE, ]$Freq
    result$TN <- freq[freq$Prediction == FALSE & freq$Reference == FALSE, ]$Freq
    result <- as.data.frame(result)
    rownames(result) <- NULL
    # print(result)
    return(result)
  }

# average the results obtained
average_result <- function(results) {
  result <- transform.numeric_data(results, FUN = function(x, na.rm = TRUE) return(mean(x, na.rm = na.rm)))
  rownames(result) <- NULL
  return(result[1,])
}

# set options for validation
validation.options <- function(
  method = "cv", 
  mode = "normal",
  # cross validation
  cv.iteration = 1, 
  cv.fold = 10,
  cv.parallel = NULL,
  cv.exception = NULL
  ) {
  cv.iteration = switch(mode, "burden" = 10, "heavy" = 5, "light" = 1, "feather" = 1, "absurd" = 1, cv.iteration)
  cv.fold = switch(mode, "burden" = 10, "heavy" = 10, "light" = 4, "feather" = 3, "absurd" = 2, cv.fold)
  # RWeka (using Java) does not support parallel processing
  # http://r.789695.n4.nabble.com/RWeka-and-multicore-package-td4678473.html
  cv.parallel.forbidden <- c("oner", "jrip", "j48", "part")
  cv.parallel <- cv.parallel[!(cv.parallel %in% cv.parallel.forbidden)]
  return(list(
      method = method,
      cv.iteration = cv.iteration,
      cv.fold = cv.fold,
      cv.parallel = cv.parallel,
      cv.exception = cv.exception
    ))
}

# set options for tuneGrid
tuner.options <- function(
  # Naive Bayes
  nb.bin = "supervised",
  nb.nbin = 10,
  nb.laplace = 0,
  # OneR
  oner.bin = "supervised",
  oner.nbin = 10,
  oner.minBucket = 6,
  # PART
  part.pruneConf = 0.25,
  part.minLeaf = 2,
  part.REP = FALSE,
  part.numFolds = 3,
  # RIPPER
  jrip.minWeight = 2.0,
  jrip.numFolds = 3,
  # (modified) Linear Regression
  lmm.threshold = 0.5,
  # Neural Network
  neuralnet.outThres = 0.5,
  neuralnet.algorithm = "rprop+",
  neuralnet.hidden = 1,
  # SVM
  svm.kernel = "radial",
  svm.cost = 1,
  # CART
  cart.cp = 0.01,
  # C4.5
  j48.pruneConf = 0.25,
  j48.minLeaf = 2,
  j48.REP = FALSE,
  j48.numFolds = 3,
  # K-Nearest Neighbor
  knn.k = 1,
  # Weighted K-Nearest Neighbor
  kknn.distance = 2,
  kknn.kernel = "optimal",
  kknn.k = 7
  ) {
  return(list(
    # Naive Bayes
    nb.bin = nb.bin,
    nb.nbin = nb.nbin,
    nb.laplace = nb.laplace,
    # OneR
    oner.bin = oner.bin,
    oner.nbin = oner.nbin,
    oner.minBucket = oner.minBucket,
    # PART
    part.pruneConf = part.pruneConf,
    part.minLeaf = part.minLeaf,
    part.REP = part.REP,
    part.numFolds = part.numFolds,
    # RIPPER
    jrip.minWeight = jrip.minWeight,
    jrip.numFolds = jrip.numFolds,
    # (modified) Linear Regression
    lmm.threshold = lmm.threshold,
    # Neural Network
    neuralnet.outThres = neuralnet.outThres,
    neuralnet.algorithm = neuralnet.algorithm,
    neuralnet.hidden = neuralnet.hidden,
    # SVM
    svm.kernel = svm.kernel,
    svm.cost = svm.cost,
    # CART
    cart.cp = cart.cp,
    # C4.5
    j48.pruneConf = j48.pruneConf,
    j48.minLeaf = j48.minLeaf,
    j48.REP = j48.REP,
    j48.numFolds = j48.numFolds,
    # K-Nearest Neighbor
    knn.k = knn.k,
    # Weighted K-Nearest Neighbor
    kknn.distance = kknn.distance,
    kknn.kernel = kknn.kernel,
    kknn.k = kknn.k
  ))
}

# generate prediction based on model constructed using training set and test against the test set
tuner.generate.prediction <-
  function(train, test, target, classifier, params, verbose) {
    features <- getrov(names(train), target)
    formula <- as.simple.formula(features, target)
    
    # CART (rpart)
    if (classifier == "cart") {
      model <- rpart::rpart(formula,
                            method = "class",
                            data = train,
                            control = rpart.control(cp = params$cart.cp)
      )
      return(predict(model, test, type = "class"))
    }
    
    # Linear Regression modified to work with 2-class problem (base)
    if (classifier == "lmm") {
      train[, target] <- as.numeric(as.logical(train[, target]))
      test[, target] <- as.numeric(as.logical(test[, target]))
      model <- lm(formula, data = train)
      pred <- NULL
      tryCatch(pred <- predict(model, test),
        warning = function(w) {
          # Handle warnings caused by linear model's rank-deficient
          # http://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
          if(w == "prediction from a rank-deficient fit may be misleading" && classifier == "lmm") {
            if(verbose == 2) {
              warning(paste("KNOWN ISSUE: caused by linear model sometimes give out the whole prediction set as TRUE or FALSE:", w, sep = "\n"))
            }
          } else {
            warning(w)
          }
          suppressWarnings(pred <<- predict(model, test))
        }
      )
      pred <- as.factor(ifelse(pred > params$lmm.threshold, TRUE, FALSE))
      return(pred)
    }

    # Neural Network (neuralnet)
    if (classifier == "neuralnet") {
      train[, target] <- as.numeric(as.logical(train[, target]))
      test[, target] <- as.numeric(as.logical(test[, target]))
      if (params$neuralnet.algorithm == "backprop") {
        model <- neuralnet::neuralnet(formula, train, hidden = params$neuralnet.hidden, algorithm=params$neuralnet.algorithm, learningrate = 0.05, linear.output = FALSE, stepmax = NEURAL_NET_STEPMAX)
      } else {
        model <- neuralnet::neuralnet(formula, train, hidden = params$neuralnet.hidden, algorithm=params$neuralnet.algorithm, stepmax = NEURAL_NET_STEPMAX)
      }
      pred <- neuralnet::compute(model, getrod(test, target))$net.result
      pred <- as.factor(ifelse(pred > params$neuralnet.outThres, TRUE, FALSE))
      return(pred)
    }

    # Support Vector Machinese (e1071)
    if (classifier == "svm") {
      wts <- nrow(train) / table(train[, target])
      model <- e1071::svm(formula, scale = FALSE, data = train, kernel = params$svm.kernel, cost = params$svm.cost, class.weights = wts)
      return(predict(model, test))
    }
    
    # OneR (RWeka)
    # do not use package OneR here it causes some conflict with RWeka::OneR
    # it takes over the predict function to use predictOneR instead
    if (classifier == "oner") {
      model <- RWeka::OneR(formula, data = train, control = Weka_control(B = params$oner.minBucket))
      return(predict(model, test))
    }
    
    # Naive Bayes (e1071)
    if (classifier == "nb") {
      model <- e1071::naiveBayes(formula = formula, data = train, laplace = params$nb.laplace)
      return(predict(model, test))
    }
    
    # RIPPER (RWeka)
    if (classifier == "jrip") {
      model <- RWeka::JRip(formula, data = train, control = Weka_control(N = params$jrip.minWeight, F = params$jrip.numFolds))
      return(predict(model, test))
    }
    
    # C4.5 (RWeka)
    if (classifier == "j48") {
      if(params$j48.REP) {
        model <- RWeka::J48(formula, data = train, control = Weka_control(M = params$j48.minLeaf, R = params$j48.REP, N = params$j48.numFolds))
      } else {
        model <- RWeka::J48(formula, data = train, control = Weka_control(C = params$j48.pruneConf, M = params$j48.minLeaf, R = params$j48.REP))
      }
      return(predict(model, test))
    }
    
    # PART (RWeka)
    if (classifier == "part") {
      if(params$part.REP) {
        model <- RWeka::PART(formula, data = train, control = Weka_control(M = params$part.minLeaf, R = params$part.REP, N = params$part.numFolds))
      } else {
        model <- RWeka::PART(formula, data = train, control = Weka_control(C = params$part.pruneConf, M = params$part.minLeaf, R = params$part.REP))
      }
      return(predict(model, test))
    }
    
    # K-Nearest Neighbor (class)
    if (classifier == "knn") {
      train.class <- train[, target]
      train <- getrod(train, target)
      test <- getrod(test, target)
      return(class::knn(train, test, train.class, k = params$knn.k))
    }

    # Weighted K-Nearest Neighbors (kknn)
    if (classifier == "kknn") {
      return(kknn::kknn(formula, train, test, distance = params$kknn.distance, kernel = params$kknn.kernel, k = params$kknn.k)$fitted.values)
    }

    warning("model is not found")
    return(NULL)
  }


# flatten a params list into a string with their names
stringify_list_with_name <- function(params, separator = " ") {
  for (i in 1:length(params)) {
    if (length(params[[i]]) > 1) {
      params[[i]] <- paste("(", paste(params[[i]], collapse = ", "), ")", sep = "")
    }
  }
  return(paste(rbind(names(params), unlist(params)), collapse = separator))
}

# a wrapper method to return a evaluator with specified validation method
tuner.generate.validator <- function(dataset,
                               target,
                               sampling,
                               classifier,
                               params,
                               validation,
                               verbose) {
    if (verbose == 1) {
      print(paste(Sys.time(), "    ", classifier, ": ", stringify_list_with_name(params), sep = ""))
    }
    result_main <- NULL
    if (!is.null(validation$cv.exception) && classifier %in% names(validation$cv.exception)) {
      validation <- validation.options(mode = validation$cv.exception[[classifier]])
    } else {
      validation$cv.exception <- NULL
    }
    # print(validation)
    for(samp_method in sampling) {
      if(validation$method == "cv") {
        for (i in 1:validation$cv.iteration) {
          splits <-
            createFolds(dataset[, target], k = validation$cv.fold, list = TRUE)
          if (classifier %in% validation$cv.parallel) {
            result <- combine_dataframe_list(alply(
              splits,
              1,
              function(i) {
                # print("parallel")
                train <- dataset[-i, ]
                train <- sampling_with_flexible_replacement(train, target, type = samp_method, shuffle = TRUE)
                test <- dataset[i, ]
                prediction <- tuner.generate.prediction(train, test, target, classifier, params, verbose)
                result <- tuner.generate.evaluation(prediction, test[, target], classifier, verbose)
                # print(result)
                return(result)
              }, 
              .parallel = TRUE)
            )
          } else {
            result <- combine_dataframe_list(lapply(splits, function(i) {
              # print("single-threaded")
              train <- dataset[-i, ]
              train <- sampling_with_flexible_replacement(train, target, type = samp_method, shuffle = TRUE)
              test <- dataset[i, ]
              prediction <- tuner.generate.prediction(train, test, target, classifier, params, verbose)
              result <- tuner.generate.evaluation(prediction, test[, target], classifier, verbose)
              # print(result)
              return(result)
            }))
          }
        }
      }
      # print(result)
      result <- average_result(result)
      result$classifier <- classifier
      for(i in 1:length(validation)) {
        if (names(validation)[i] != "cv.parallel") {
          result[, names(validation)[i]] <- validation[[i]]
        }
      }
      result$sampling <- samp_method
      for(i in 1:length(params)) {
        if (length(params[[i]]) > 1) {
          params[[i]] <- paste("(", paste(params[[i]], collapse = ", "), ")", sep = "")
        }
        if (names(params)[i] == "neuralnet.hidden") {
          result[, names(params)[i]] <- as.character(params[[i]])
        } else {
          result[, names(params)[i]] <- params[[i]]
        }
      }
      result_main <- rbind(result_main, result)
    }
    return(result_main)
}

# generate the classifier based on the parameter tune grid
tuner.generate.classifier <- function(data, target, sampling, classifier, validation, tuneGrid, verbose) {
  results <- NULL
  features <- getrov(names(data), target)
  formula <- as.simple.formula(features, target)
  
  # CART (rpart)
  if (classifier == "cart") {
    data[, target] <- as.factor(data[, target])
    cp.list <- tuneGrid$cart.cp
    for (cp in cp.list) {
      params = list(cart.cp = cp)
      results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
    }
  }
  
  # Linear Regression modified to work with 2-class problem (base)
  if (classifier == "lmm") {
    data <- numerize_dataframe(data)
    data[, target] <- as.logical(data[, target])
    data[, target] <- as.factor(data[, target])
    threshold.list <- tuneGrid$lmm.threshold
    for (threshold in threshold.list) {
      params = list(lmm.threshold = threshold)
      results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
    }
  }

  # Neural Network (neuralnet)
  if (classifier == "neuralnet") {
    data <- numerize_dataframe(data)
    data[, target] <- as.logical(data[, target])
    data[, target] <- as.factor(data[, target])
    algorithm.list <- tuneGrid$neuralnet.algorithm
    hidden.list <- tuneGrid$neuralnet.hidden
    outThres.list <- tuneGrid$neuralnet.outThres
    for (algorithm in algorithm.list) {
      for (outThres in outThres.list) {
        for (hidden in hidden.list) {
          params = list(neuralnet.algorithm = algorithm, neuralnet.hidden = hidden, neuralnet.outThres = outThres)
          results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
        }
      }
    }
  }

  # Support Vector Machines (e1071)
  if (classifier == "svm") {
    data <- transform.numeric_data(data, FUN = standardize)
    data <- numerize_dataframe(data)
    data[, target] <- as.logical(data[, target])
    data[, target] <- as.factor(data[, target])
    cost.list <- tuneGrid$svm.cost
    kernel.list <- tuneGrid$svm.kernel
    for (cost in cost.list) {
      for (kernel in kernel.list) {
        params = list(svm.cost = cost, svm.kernel = kernel)
        results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
      }
    }
  }

  # OneR (RWeka)
  if (classifier == "oner") {
    data[, target] <- as.factor(data[, target])
    bin.list <- tuneGrid$oner.bin
    nbin.list <- tuneGrid$oner.nbin
    minBucket.list <- tuneGrid$oner.minBucket
    for (bin in bin.list) {
      for (minBucket in minBucket.list) {
        if (bin %in% c("supervised")) {
            data <- RWeka::Discretize(formula, data)
            params = list(oner.bin = bin, oner.minBucket = minBucket)
            data <- all.logical_to_factor(data)
            results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
        }
        if (bin %in% c("frequency", "interval")) {
          for (nbin in nbin.list) {
            data <- transform.numeric_data(data, function(x, na.rm = TRUE) return(arules::discretize(x, method = bin, categories = nbin)))
            params = list(oner.bin = bin, oner.nbin = nbin, oner.minBucket = minBucket)
            data <- all.logical_to_factor(data)
            results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
          }
        }
      }
    }
  }
  
  # Naive Bayes (e1071)
  if (classifier == "nb") {
    data[, target] <- as.factor(data[, target])
    bin.list <- tuneGrid$nb.bin
    nbin.list <- tuneGrid$nb.nbin
    laplace.list <- tuneGrid$nb.laplace
    for (bin in bin.list) {
      for(laplace in laplace.list) {
        if (bin %in% c("supervised")) {
            data <- RWeka::Discretize(formula, data)
            params = list(nb.bin = bin, nb.laplace = laplace)
            data <- all.logical_to_factor(data)
            results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
        }
        if (bin %in% c("frequency", "interval")) {
          for (nbin in nbin.list) {
            data <- transform.numeric_data(data, function(x, na.rm = TRUE) return(arules::discretize(x, method = bin, categories = nbin)))
            params = list(nb.bin = bin, nb.nbin = nbin, nb.laplace = laplace)
            data <- all.logical_to_factor(data)
            results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
          }
        }
      }
    }
  }
  
  # RIPPER (RWeka)
  if (classifier == "jrip") {
    data[, target] <- as.factor(data[, target])
    minWeight.list <- tuneGrid$jrip.minWeight
    numFolds.list <- tuneGrid$jrip.numFolds
    for (minWeight in minWeight.list) {
      for (numFolds in numFolds.list) {
        params = list(jrip.minWeight = minWeight, jrip.numFolds = numFolds)
        results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
      }
    }
  }
  
  # C4.5 (RWeka)
  if (classifier == "j48") {
    data[, target] <- as.factor(data[, target])
    pruneConf.list <- tuneGrid$j48.pruneConf
    minLeaf.list <- tuneGrid$j48.minLeaf
    REP.list <- tuneGrid$j48.REP
    numFolds.list <- tuneGrid$j48.numFolds
    for (pruneConf in pruneConf.list) {
      for (minLeaf in minLeaf.list) {
        for (REP in REP.list) {
          if (REP) {
            for(numFolds in numFolds.list) {
              params = list(j48.minLeaf = minLeaf, j48.REP = REP, j48.numFolds = numFolds)
              results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
            }
          } else {
            params = list(j48.pruneConf = pruneConf, j48.minLeaf = minLeaf, j48.REP = REP)
            results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
          }
        }
      }
    }
  }
  
  # PART (RWeka)
  if (classifier == "part") {
    data[, target] <- as.factor(data[, target])
    pruneConf.list <- tuneGrid$part.pruneConf
    minLeaf.list <- tuneGrid$part.minLeaf
    REP.list <- tuneGrid$part.REP
    numFolds.list <- tuneGrid$part.numFolds
    for (pruneConf in pruneConf.list) {
      for (minLeaf in minLeaf.list) {
        for (REP in REP.list) {
          if (REP) {
            for(numFolds in numFolds.list) {
              params = list(part.minLeaf = minLeaf, part.REP = REP, part.numFolds = numFolds)
              results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
            }
          } else {
            params = list(part.pruneConf = pruneConf, part.minLeaf = minLeaf, part.REP = REP)
            results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
          }
        }
      }
    }
  }
  
  # K-Nearest Neighbor (class)
  # requires all data to be numeric so distance metric works
  # need to numerize everything except leaving the target as
  # a factor
  if (classifier == "knn") {
    data <- numerize_dataframe(data)
    data[, target] <- as.logical(data[, target])
    data[, target] <- as.factor(data[, target])
    k.list <- tuneGrid$knn.k
    for (k in k.list) {
      params = list(knn.k = k)
      results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
    }
  }

  # Weighted K-Nearest Neighbor (kknn)
  # same requirement as K-Nearest Neighbors
  if (classifier == "kknn") {
    data <- numerize_dataframe(data)
    data[, target] <- as.logical(data[, target])
    data[, target] <- as.factor(data[, target])
    distance.list <- tuneGrid$kknn.distance
    kernel.list <- tuneGrid$kknn.kernel
    k.list <- tuneGrid$kknn.k
    for (distance in distance.list) {
      for (kernel in kernel.list) {
        for (k in k.list) {
          params = list(kknn.distance = distance, kknn.kernel = kernel, kknn.k = k)
          results <- bind_rows(results, tuner.generate.validator(data, target, sampling, classifier, params, validation, verbose))
        }
      }
    }
  }
  
  return(results)
}

# the main tuner
# verbose: 0: quite, 1: debug, 2: known issue warnings
tuner.param_tune <-
  function(data,
           target,
           preprocess = NULL,
           sampling = c("none"),
           classifiers,
           validation = validation.options(),
           tuneGrid = tuner.options(),
           info = "",
           verbose = 0) {
    # transform all numeric data if needed
    if (!is.null(preprocess)) {
      if (preprocess == "normalize") {
        data <- transform.numeric_data(data, FUN = normalize)
      } else if (preprocess == "standardize") {
        data <- transform.numeric_data(data, FUN = standardize)
      }
    }
    # get the result
    results <- NULL
    for (classifier in classifiers) {
      results <- bind_rows(results, tuner.generate.classifier(data, target, sampling, classifier, validation, tuneGrid, verbose))
    }
    results$info <- info 
    results$preprocess <- preprocess
    return(results)
  }