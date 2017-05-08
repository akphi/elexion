############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities supporting for apps

############################################################
##                        METHOD                          ##
############################################################

# use (potential) all the dataset to build the model 
retrieve_model <- function(guide, data, target, info = "", preprocess = TRUE) {
  result <- NULL
  result$info <- info
  evaluation <- guide[, names(guide) %in% c(
      "accuracy",
      "kappa",
      "misclassification",
      "recall.TRUE",
      "recall.FALSE",
      "precision.TRUE",
      "precision.FALSE",
      "prevalence.TRUE",
      "prevalence.FALSE",
      "detection_prevalence.TRUE",
      "detection_prevalence.FALSE",
      "f1.TRUE",
      "f1.FALSE",
      "TP",
      "FP",
      "FN",
      "TN"
    )]
  # exclude features
  data <- data[, names(data) %notin% strsplit(guide$exclude, ", ")[[1]]]
  # preprocess the data
  if (preprocess) {
    if (!is.null(guide$preprocess)) {
      if (guide$preprocess == "normalize") {
        data <- transform.numeric_data(data, FUN = normalize)
      } else if (guide$preprocess == "standardize") {
        data <- transform.numeric_data(data, FUN = standardize)
      }
    }
  }
  # sampling the data
  data <- sampling_with_flexible_replacement(data, target, type = guide$sampling, shuffle = TRUE)
  # transform data
  data[, target] <- as.factor(data[, target])
  # train by model

  # setup
  classifier <- guide$classifier
  features <- getrov(names(data), target)
  formula <- as.simple.formula(features, target)
  
  # print(classifier)

  # modelling
  model <- NULL
  parameter <- NULL

  # CART (rpart)
  if (classifier == "cart") {
    data[, target] <- as.factor(data[, target])
    model <- rpart::rpart(formula, method = "class", data = data, control = rpart.control(cp = guide$cart.cp))
    parameter <- list(cart.cp = guide$cart.cp)
  }
  
  # Linear Regression modified to work with 2-class problem (base)
  if (classifier == "lmm") {
    data[, target] <- as.logical(data[, target])
    data <- numerize_dataframe(data)
    formula <- as.simple.formula(getrov(names(data), target), target)
    model <- lm(formula, data = data)
    parameter <- list(lmm.threshold = guide$lmm.threshold)
  }

  # Support Vector Machines (e1071)
  if (classifier == "svm") {
    data <- transform.numeric_data(data, FUN = standardize)
    data[, target] <- as.logical(data[, target])
    data <- numerize_dataframe(data)
    data[, target] <- as.logical(data[, target])
    data[, target] <- as.factor(data[, target])
    formula <- as.simple.formula(getrov(names(data), target), target)
    wts <- nrow(data) / table(data[, target])
    model <- e1071::svm(formula, scale = FALSE, data = data, kernel = guide$svm.kernel, cost = guide$svm.cost, class.weights = wts)
    parameter <- list(svm.kernel = guide$svm.kernel, svm.cost = guide$svm.cost)
  }

  # Neural Network (neuralnet)
  if (classifier == "neuralnet") {
    data[, target] <- as.logical(data[, target])
    data <- numerize_dataframe(data)
    formula <- as.simple.formula(getrov(names(data), target), target)
    # retrive the hidden layers' configuration
    layers_conf <- if(substring(guide$neuralnet.hidden, 0, 1) == "(") {
      layers_conf <- as.numeric(as.vector(strsplit(substring(guide$neuralnet.hidden, 2, nchar(guide$neuralnet.hidden)-1), ",")[[1]]))
    } else {
      layers_conf <- as.numeric(guide$neuralnet.hidden)
    }
    model <- neuralnet(formula, data, hidden = layers_conf, algorithm = guide$neuralnet.algorithm, stepmax = NEURAL_NET_STEPMAX)
    parameter <- list(neuralnet.outThres = guide$neuralnet.outThres, neuralnet.hidden = guide$neuralnet.hidden, neuralnet.algorithm = guide$neuralnet.algorithm)
  }
  
  # OneR (RWeka)
  if (classifier == "oner") {
    data[, target] <- as.factor(data[, target])
    # discretize data
    if (guide$oner.bin %in% c("supervised")) {
      data <- RWeka::Discretize(formula, data)
    }
    if (guide$nb.bin %in% c("frequency", "interval")) {
      data <- transform.numeric_data(data, function(x, na.rm = TRUE) return(arules::discretize(x, method = guide$oner.bin, categories = guide$oner.nbin)))
    }
    model <- RWeka::OneR(formula, data = data, control = Weka_control(B = guide$oner.minBucket))
    parameter <- list(oner.bin = guide$oner.bin, oner.nbin = guide$oner.nbin, oner.minBucket = guide$oner.minBucket)
  }
  
  # Naive Bayes (e1071)
  if (classifier == "nb") {
    data[, target] <- as.factor(data[, target])
    # discretize data
    if (guide$nb.bin %in% c("supervised")) {
      data <- RWeka::Discretize(formula, data)
    }
    if (guide$nb.bin %in% c("frequency", "interval")) {
      data <- transform.numeric_data(data, function(x, na.rm = TRUE) return(arules::discretize(x, method = guide$nb.bin, categories = guide$nb.nbin)))
    }
    model <- e1071::naiveBayes(formula = formula, data = data, laplace = guide$nb.laplace)
    parameter <- list(nb.bin = guide$nb.bin, nb.nbin = guide$nb.nbin, nb.laplace = guide$nb.laplace)
  }
  
  # RIPPER (RWeka)
  if (classifier == "jrip") {
    data[, target] <- as.factor(data[, target])
    model <- RWeka::JRip(formula, data = data, control = Weka_control(N = guide$jrip.minWeight, F = guide$jrip.numFolds))
    parameter <- list(jrip.minWeight = guide$jrip.minWeight, jrip.numFolds = guide$jrip.numFolds)
  }
  
  # C4.5 (RWeka)
  if (classifier == "j48") {
    data[, target] <- as.factor(data[, target])
    if(guide$j48.REP) {
      model <- RWeka::J48(formula, data = data, control = Weka_control(M = guide$j48.minLeaf, R = guide$j48.REP, N = guide$j48.numFolds))
    } else {
      model <- RWeka::J48(formula, data = data, control = Weka_control(C = guide$j48.pruneConf, M = guide$j48.minLeaf, R = guide$j48.REP))
    }
    parameter <- list(j48.pruneConf = guide$j48.pruneConf, j48.minLeaf = guide$j48.minLeaf, j48.REP = guide$j48.REP, j48.numFolds = guide$j48.numFolds)
  }
  
  # PART (RWeka)
  if (classifier == "part") {
    data[, target] <- as.factor(data[, target])
    if(guide$part.REP) {
      model <- RWeka::PART(formula, data = data, control = Weka_control(M = guide$part.minLeaf, R = guide$part.REP, N = guide$part.numFolds))
    } else {
      model <- RWeka::PART(formula, data = data, control = Weka_control(C = guide$part.pruneConf, M = guide$part.minLeaf, R = guide$part.REP))
    }
    parameter <- list(part.pruneConf = guide$part.pruneConf, part.minLeaf = guide$part.minLeaf, part.REP = guide$part.REP, part.numFolds = guide$part.numFolds)
  }
  
  # K-Nearest Neighbor (class)
  if (classifier == "knn") {
    # no real model
    parameter <- list(knn.k = guide$knn.k)
  }

  # Weighted K-Nearest Neighbor (kknn)
  if (classifier == "kknn") {
    # no real model
    parameter <- list(kknn.distance = guide$kknn.distance, kknn.kernel = guide$kknn.kernel, kknn.k = guide$kknn.k)
  }

  result$model <- model
  result$parameter <- parameter
  return(result)
}

# slice the results by type and sampling
slice_by_type_and_sampling <- function(dataset, types = NULL, samplings = NULL) {
  result <- NULL
  result$total <- dataset
  if (is.null(types) && is.null(samplings)) {
    return(result)
  }
  if (is.null(types)) {
    for (sampling in samplings) {
      result[[sampling]] <- dataset[dataset$sampling == sampling, ]
    }
  } else {
    for (type in types) {
      if (is.null(samplings)) {
        result[[type]] <- dataset[dataset$type == type, ]
      } else {
        temp <- NULL
        for (sampling in samplings) {
          temp[[sampling]] <- dataset[dataset$sampling == sampling & dataset$type == type, ]
        }
        result[[type]] <- temp
      }
    }
  }
  return(result)
}

# boxplot a single graph
boxplot_singlet <- function(formula, slices, ylim = c(0.75,1), xlab = "", ylab = "") {
  par(bg = BACKGROUND_COLOR)
  boxplot(formula, slices$total, outline = FALSE, ylim = ylim, main = "Total", xlab = xlab, ylab = ylab)
}

# boxplot 4 graphs
boxplot_quartet <- function(formula, slices, ylim = c(0.75,1)) {
  par(bg = BACKGROUND_COLOR)
  op = par(mfrow=c(2, 2))
  boxplot(formula, slices$total, outline = FALSE, ylim = ylim, main = "Total")
  slices$total <- NULL
  for (i in 1:length(slices)) {
    boxplot(formula, slices[[i]], outline = FALSE, ylim = ylim, main = paste("[", names(slices)[i], "]", sep = ""))
  }
  par(op)
}

# boxplot 9 graphs
boxplot_nonet <- function(formula, slices, ylim = c(0.75,1), las = 1, medlwd = 1) {
  par(bg = BACKGROUND_COLOR)
  op = par(mfrow=c(3, 3))
  slices$total <- NULL
  for (i in 1:length(slices)) {
    for (j in 1:length(slices[[i]])) {
      boxplot(formula, slices[[i]][[j]], outline = FALSE, ylim = ylim, main = paste("[", names(slices)[i], ", ", names(slices[[i]])[j], "]", sep = ""), las = las, medlwd = medlwd)
    }
  }
  par(op)
}


# Obtain the best for each slice
best_of_each_slice <- function(slices, by = "classifier", metric, base = NULL) {
  slices$total <- NULL
  for (i in 1:length(slices)) {
    for (j in 1:length(slices[[i]])) {
      slices[[i]][[j]] <- best_models(slices[[i]][[j]], by, metric, base)
    }
  }
  return(slices)
}

# Obtain the best (metric) models for each level of the criteria (by)
best_models <- function(slice, by = "classifier", metric, base = NULL) {
  slice[, by] <- factor(slice[, by])
  result <- NULL
  for (level in levels(slice[, by])) {
    data <- slice[slice[, by] == level, ]
    result <- rbind(result, data[which.max(data[, metric]),])
  }
  if (by == "classifier" && !is.null(base)) {
    result <- result[result$accuracy > result[result[, by] == base,]$accuracy, ]
  }
  return(result)
}

# clean up the result 
sanitize_result <- function(result, to_sanitize = c("misclassification", "recall.TRUE", "precision.FALSE", "prevalence.TRUE", "prevalence.FALSE", "detection_prevalence.TRUE", "detection_prevalence.FALSE", "f1.TRUE", "f1.FALSE", "TP", "FP", "FN", "TN", "info", "type", "exclude")) {
  result <- result[, names(result) %notin% to_sanitize]
  result <- result[, colSums(!is.na(result)) > 0]
  return(result)
}