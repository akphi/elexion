############################################################
##                      DESCRIPTION                       ##
############################################################

# modelling using the analytic base table

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
source("misc.R")
install_load("doParallel")
registerDoParallel(NUMBER_OF_CORE_PARALLEL)

############################################################
##                      INITIALIZE                        ##
############################################################

log.write("start modelling script")
# allow the same result as in the paper by setting the seed
if (REPRODUCIBILITY) {
  # http://stackoverflow.com/questions/34946177/r-llply-fully-reproducible-results-in-parallel
  RNGkind(RNG_KIND)
  set.seed(RNG_SEED)
  mc.reset.stream()
}

############################################################
##                          LOAD                          ##
############################################################

data_orig <-
  read.csv(mp(DATA_SELECTED_DIR, "data.csv"), stringsAsFactors = TRUE)
meta_orig <-
  read.csv(mp(DATA_SELECTED_DIR, "meta.csv"), stringsAsFactors = FALSE)
# set MAIN
data_seed <- data_orig
meta_seed <- meta_orig

############################################################
##                      TRANSFORM                         ##
############################################################

data_seed$fips_state <- as.factor(data_seed$fips_state)

############################################################
##                       DATASET                          ##
############################################################
# subsetting different features of the original dataset

# +----------------------------------+
# |           ALL FEATURES           |
# +----------------------------------+
dataset.full <- NULL
dataset.full$exclude <- ""
dataset.full$sampling <- "none"
dataset.full$data <- data_seed

# +----------------------------------+
# |      EXCLUDE elec_rep12_win      |
# +----------------------------------+
to_exclude <- c("elec_rep12_win")
dataset.ex_r12 <- NULL
dataset.ex_r12$exclude <- paste(to_exclude, collapse = ", ")
dataset.ex_r12$data <- dataset.full$data[, names(dataset.full$data) %notin% to_exclude]

# +----------------------------------+
# | EXC. elec_rep12_win, fips_state  |
# +----------------------------------+
to_exclude <- c("elec_rep12_win", "fips_state")
dataset.ex_r12.fips <- NULL
dataset.ex_r12.fips$exclude <- paste(to_exclude, collapse = ", ")
dataset.ex_r12.fips$data <- dataset.full$data[, names(dataset.full$data) %notin% to_exclude]

############################################################
##                       MODELLING                        ##
############################################################

# @MD005
# build models and tune parameters
# use all datasets
data_list <- list(
  dataset.full,
  dataset.ex_r12,
  dataset.ex_r12.fips
)
sampling_flavors <- c("under", "none", "over")
verbose_level <- 1
# start the iteration
result_modelling <- NULL
log.write("start bulk modelling")
for (data in data_list) {
  result_temp <- NULL
  tuneGrid <- tuner.options(
    # Naive Bayes (45)
    nb.bin = c("supervised", "frequency"),
    # nb.nbin = seq(10,80,10), # DEF 10 -> NUM 8
    nb.laplace = seq(0,4,1), # DEF 0 -> NUM 5
    # OneR (8)
    oner.bin = c("supervised", "frequency"),
    oner.nbin = seq(10,80,10), #DEF 10 -> NUM 8
    oner.minBucket = 6, #DEF 6 -> NUM 1
    # PART (30)
    part.pruneConf = 0.25, # DEF 0.25 -> NUM 1
    part.minLeaf = seq(1,15,1), # DEF 2 -> NUM 15
    part.REP = c(FALSE, TRUE), # DEF FALSE -> NUM 2
    part.numFolds = 3, # DEF 3 -> NUM 1
    # RIPPER (21)
    jrip.minWeight = seq(0,20,1), # DEF 2.0 -> NUM 21
    jrip.numFolds = 3, # DEF 3 -> NUM 1
    # Linear Regression MOD (20)
    lmm.threshold = seq(0.3,0.8,0.05), # DEF 0.5 -> NUM 20
    # Neural Network (12)
    neuralnet.outThres = 0.5, # DEF 0.5 -> NUM 1
    neuralnet.algorithm = c("rprop+", "rprop-"), # DEF rprop+ -> NUM 2
    neuralnet.hidden = list(c(2,1), c(3,2), c(4,3), c(5,4), c(5,2), c(7,5)), # DEF 1 -> NUM 6
    # SVM (52)
    svm.kernel = c("linear", "polynomial", "radial", "sigmoid"), # DEF radial -> NUM 4
    svm.cost = seq(0.1,1.3,0.1), # DEF 1 -> NUM 13
    # CART (21)
    cart.cp = append(c(0.01), seq(0,1,0.05)), # DEF 0.01 -> NUM 21
    # C4.5 (30)
    j48.pruneConf = 0.25, # DEF 0.25 -> NUM 1
    j48.minLeaf = seq(1,15,1), # DEF 2 -> NUM 15
    j48.REP = c(FALSE, TRUE), # DEF FALSE -> NUM 2
    j48.numFolds = 3, # DEF 3 -> NUM 1
    # K-Nearest Neighbor (68)
    knn.k = append(seq(1,19,2),seq(259,399,5)), # DEF 1 -> NUM 39
    # Weighted K-Nearest Neighbor (61)
    kknn.distance = 2, # DEF 2 -> NUM 1
    kknn.kernel = "optimal",
    kknn.k = append(seq(1,10,1),seq(20,70,1)) # DEF 7 -> NUM 61
  )
  # under sampling
  if ("under" %in% sampling_flavors) {
    if (verbose_level > 0) {
      print(paste(Sys.time(), "    UNDER SAMPLING: ", data$exclude, sep = ""))
    }
    result_temp <- bind_rows(result_temp, tuner.param_tune(data = data$data,
              target = "elec_rep16_win",
              preprocess = "normalize",
              sampling = c("under"),
              classifiers = c("nb", "oner", "lmm", "neuralnet", "svm", "jrip", "part", "j48", "cart", "knn", "kknn"),
              validation = validation.options(
                cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn"), 
                cv.exception = list(neuralnet = "light", knn = "light", kknn = "light")),
              tuneGrid = tuneGrid,
              info = "",
              verbose = verbose_level))
  }
  # no sampling
  if ("none" %in% sampling_flavors) {
    if (verbose_level > 0) {
      print(paste(Sys.time(), "    NO SAMPLING: ", data$exclude, sep = ""))
    }
    result_temp <- bind_rows(result_temp, tuner.param_tune(data = data$data,
              target = "elec_rep16_win",
              preprocess = "normalize",
              sampling = c("none"),
              classifiers = c("nb", "oner", "lmm", "neuralnet", "svm", "jrip", "part", "j48", "cart", "knn", "kknn"),
              validation = validation.options(
                cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn"),
                cv.exception = list(neuralnet = "absurd", knn = "light", kknn = "light")),
              tuneGrid = tuneGrid,
              info = "",
              verbose = verbose_level))
  }
  # over sampling
  if ("over" %in% sampling_flavors) {
    if (verbose_level > 0) {
      print(paste(Sys.time(), "    OVER SAMPLING: ", data$exclude, sep = ""))
    }
    result_temp <- bind_rows(result_temp, tuner.param_tune(data = data$data,
              target = "elec_rep16_win",
              preprocess = "normalize",
              sampling = c("over"),
              classifiers = c("nb", "oner", "lmm", "neuralnet", "svm", "jrip", "part", "j48", "cart", "knn", "kknn"),
              validation = validation.options(
                cv.iteration = 1, cv.fold = 4, 
                cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn"), 
                cv.exception = list(neuralnet = "absurd")),
              tuneGrid = tuneGrid,
              info = "",
              verbose = verbose_level))
  }
  result_temp$exclude <- data$exclude
  result_modelling <- bind_rows(result_modelling, result_temp)
}
log.write("end bulk modelling")

############################################################
##                   CATEGORY SELECTION                   ##
############################################################

# @MD006
# attempt to find the best combination of category for each metric
log.write("start category selection")
# use over-sampling data
data_ge <- dataset.ex_r12.fips$data
meta_ge <- meta_seed[meta_seed$var_name %notin% c("elec_rep16_win", "elec_rep12_win", "fips_state"),]
categories <- levels(as.factor(meta_ge$category))
# group attribute by their categories
by_category <- list()
for (i in categories) {
  by_category[[i]] <- meta_ge[meta_ge$category == i,]$var_name
}
# create all subsets to conduct exhaustive search
subset <- list()
for (i in 1:length(categories)) {
  subset <- append(subset, combn(categories, i , simplify = FALSE))
}
# start to search through all the subsets
result_ge <- NULL
for (i in subset) {
  data_ge_temp <-
    data_ge[, meta_ge[meta_ge$category %in% i,]$var_name]
  data_ge_temp$elec_rep16_win <- data_ge$elec_rep16_win
  result_ge_temp <- tuner.param_tune(
    data = data_ge_temp,
    target = "elec_rep16_win",
    preprocess = "normalize",
    sampling = c("under"),
    classifiers = c("nb", "lmm", "svm", "jrip", "part", "j48", "cart", "knn", "kknn"),
    validation = validation.options(
      method = "cv",
      # cross validation
      cv.iteration = 1,
      cv.fold = 5
    ),
    tuneGrid = tuner.options(),
    info = "",
    verbose = 0
  )
  result_ge_temp <- average_result(result_ge_temp)
  result_ge_temp[, categories] <- FALSE
  result_ge_temp[, i] <- TRUE
  result_ge <-
    bind_rows(result_ge, result_ge_temp[, append(c(
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
    ), categories)])
}
log.write("end category selection")

############################################################
##                        EXPORT                          ##
############################################################

write.csv(
  result_modelling,
  file = mp(RES_MODEL_TUNED_DIR, "models.csv"),
  row.names = FALSE
)
write.csv(
  result_ge,
  file = mp(RES_MODEL_TUNED_DIR, "categories.csv"),
  row.names = FALSE
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end modelling script")
# stop parallel cores
stopImplicitCluster()
reset.work_space()