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

log.write("start model probing script")
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
to_exclude <- c("elec_rep12_win", "fips_state")
dataset.ex_r12.fips <- NULL
dataset.ex_r12.fips$exclude <- paste(to_exclude, collapse = ", ")
dataset.ex_r12.fips$data <- data_seed[, names(data_seed) %notin% to_exclude]

############################################################
##                       MODELLING                        ##
############################################################

# build models and tune parameters
data_list <- list(
  dataset.ex_r12.fips
)
probing_sampling <- c("over")
verbose_level <- 1
# start the iteration
result_probing <- NULL
for (data in data_list) {
  result_temp <- NULL
  tuneGrid <- tuner.options(
    # Neural Network (16)
    neuralnet.outThres = 0.5, # DEF 0.5 -> NUM 1
    neuralnet.algorithm = c("rprop+"), # DEF rprop+ -> NUM 1
    neuralnet.hidden = list(
        2, 4, 10, 20, c(2,1), c(2,2), c(3,2), 
        c(3,4), c(4,3), c(5,2), c(5,4), c(7,5), 
        c(8,7), c(10,10), c(12,8), c(12,12), 
        c(15,20), c(20,20), c(25,30), c(30,30)
      ), # DEF 1 -> NUM 16
    # K-Nearest Neighbor (240)
    knn.k = seq(1,499,2), # DEF 1 -> NUM 250
    # Weighted K-Nearest Neighbor (100)
    kknn.distance = 2, # DEF 2 -> NUM 1
    kknn.kernel = "optimal",
    kknn.k = seq(1,100,1) # DEF 7 -> NUM 100
  )

  # @MD001
  # KNN
  if ( verbose_level == 1) {
    print("KNN")
  }
  knn.under <- tuner.param_tune(data = data$data,
            target = "elec_rep16_win",
            preprocess = "normalize",
            sampling = c("under"),
            classifiers = c("knn"),
            validation = validation.options(
              cv.iteration = 1,
              cv.fold = 4,
              cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn")
            ),
            tuneGrid = tuneGrid,
            info = "",
            verbose = verbose_level)
  knn.over <- tuner.param_tune(data = data$data,
            target = "elec_rep16_win",
            preprocess = "normalize",
            sampling = c("over"),
            classifiers = c("knn"),
            validation = validation.options(
              cv.iteration = 1, 
              cv.fold = 4, 
              cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn"), 
              cv.exception = list(neuralnet = "absurd")),
            tuneGrid = tuneGrid,
            info = "",
            verbose = verbose_level)
  ENABLE_MODEL_PARAMETER_PROBING

  # @MD002
  # KKNN
  if ( verbose_level == 1) {
    print("KKNN")
  }
  kknn.under <- tuner.param_tune(data = data$data,
            target = "elec_rep16_win",
            preprocess = "normalize",
            sampling = c("under"),
            classifiers = c("kknn"),
            validation = validation.options(
              cv.iteration = 1,
              cv.fold = 4,
              cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn")
            ),
            tuneGrid = tuneGrid,
            info = "",
            verbose = verbose_level)
  kknn.over <- tuner.param_tune(data = data$data,
            target = "elec_rep16_win",
            preprocess = "normalize",
            sampling = c("over"),
            classifiers = c("kknn"),
            validation = validation.options(
              cv.iteration = 1, 
              cv.fold = 4, 
              cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn"), 
              cv.exception = list(neuralnet = "absurd")),
            tuneGrid = tuneGrid,
            info = "",
            verbose = verbose_level)

  # @MD003
  # Neural Network
  if ( verbose_level == 1) {
    print("NEURAL NETWORK")
  }
  neuralnet.under <- tuner.param_tune(data = data$data,
            target = "elec_rep16_win",
            preprocess = "normalize",
            sampling = c("under"),
            classifiers = c("neuralnet"),
            validation = validation.options(
              cv.iteration = 1,
              cv.fold = 4,
              cv.parallel = c("nb", "lmm", "neuralnet", "svm", "cart", "knn", "kknn")
            ),
            tuneGrid = tuneGrid,
            info = "",
            verbose = verbose_level)
}
result_probing <- bind_rows(knn.under, knn.over, kknn.under, kknn.over, neuralnet.under)

############################################################
##                        VISUALIZE                       ##
############################################################

# @MD004
# KNN
png(
  mp(RES_MODEL_PROBED_PLOT_DIR, "knn.k_under.sampling.png"),
  width = LG_LENGTH,
  height = LG_LENGTH
)
with(knn.under, plot(knn.under$knn.k, knn.under$accuracy, cex = 0.6, bty = "o", xlab = "k", ylab = "Accuracy", main = "Accuracy vs. Number of neighbor (k)"))
dev.off()
png(
  mp(RES_MODEL_PROBED_PLOT_DIR, "knn.k_over.sampling.png"),
  width = LG_LENGTH,
  height = LG_LENGTH
)
with(knn.over, plot(knn.over$knn.k, knn.over$accuracy, cex = 0.6, bty = "o", xlab = "k", ylab = "Accuracy", main = "Accuracy vs. Number of neighbor (k)"))
dev.off()
# KKNN
png(
  mp(RES_MODEL_PROBED_PLOT_DIR, "kknn.k_under.sampling.png"),
  width = LG_LENGTH,
  height = LG_LENGTH
)
with(kknn.under, plot(kknn.under$kknn.k, kknn.under$accuracy, cex = 0.6, bty = "o", xlab = "k", ylab = "Accuracy", main = "Accuracy vs. Number of neighbor (k)"))
dev.off()
png(
  mp(RES_MODEL_PROBED_PLOT_DIR, "kknn.k_over.sampling.png"),
  width = LG_LENGTH,
  height = LG_LENGTH
)
with(kknn.over, plot(kknn.over$kknn.k, kknn.over$accuracy, cex = 0.6, bty = "o", xlab = "k", ylab = "Accuracy", main = "Accuracy vs. Number of neighbor (k)"))
dev.off()
# Neural Network
png(
  mp(RES_MODEL_PROBED_PLOT_DIR, "neuralnet_under.sampling.png"),
  width = MD_LENGTH,
  height = MD_LENGTH
)
plot(
  neuralnet.under$accuracy, 
  bty="n", ylab = "Accuracy", 
  main = "Hidden Layers Node Configuration", 
  xaxt='n', xlab="", 
  ylim = c(round_any(min(neuralnet.under$accuracy),0.01,f=floor),round_any(max(neuralnet.under$accuracy),0.01,f=ceiling))
)
text(neuralnet.under$accuracy, labels=neuralnet.under$neuralnet.hidden, cex = 0.7, pos = 3)
dev.off()

############################################################
##                        EXPORT                          ##
############################################################

write.csv(
  knn.under,
  file = mp(RES_MODEL_PROBED_DIR, "knn.under.csv"),
  row.names = FALSE
)
write.csv(
  knn.over,
  file = mp(RES_MODEL_PROBED_DIR, "knn.over.csv"),
  row.names = FALSE
)
write.csv(
  kknn.under,
  file = mp(RES_MODEL_PROBED_DIR, "kknn.under.csv"),
  row.names = FALSE
)
write.csv(
  kknn.over,
  file = mp(RES_MODEL_PROBED_DIR, "kknn.over.csv"),
  row.names = FALSE
)
write.csv(
  neuralnet.under,
  file = mp(RES_MODEL_PROBED_DIR, "neuralnet.under.csv"),
  row.names = FALSE
)
write.csv(
  result_probing,
  file = mp(RES_MODEL_PROBED_DIR, "params_probed.csv"),
  row.names = FALSE
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end model probing script")
# stop parallel cores
stopImplicitCluster()
reset.work_space()