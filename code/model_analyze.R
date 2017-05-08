############################################################
##                      DESCRIPTION                       ##
############################################################

# analyze modelling model_guides

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
source("misc.R")
install_load(c(
  "arules",
  "arulesViz")
)
install_load("doParallel")
registerDoParallel(NUMBER_OF_CORE_PARALLEL)

############################################################
##                      INITIALIZE                        ##
############################################################

log.write("start model analyze script")
if (REPRODUCIBILITY) {
  # http://stackoverflow.com/questions/34946177/r-llply-fully-reproducible-results-in-parallel
  RNGkind(RNG_KIND)
  set.seed(RNG_SEED)
  mc.reset.stream()
}
RUN_SPECIAL <- TRUE
ENHANCE_INTERPRETABILITY <- TRUE
THRESHOLD_ACCURACY_CATEGORY_BASKET <- 0.85
THRESHOLD_KAPPA_CATEGORY_BASKET <- 0.6

############################################################
##                          LOAD                          ##
############################################################

data_seed <- read.csv(mp(DATA_SELECTED_DIR, "data.csv"), stringsAsFactors = TRUE)
meta_seed <- read.csv(mp(DATA_SELECTED_DIR, "meta.csv"), stringsAsFactors = FALSE)
model_seed <- read.csv(mp(RES_MODEL_TUNED_DIR, "models.csv"), stringsAsFactors = FALSE)
category_seed <- read.csv(mp(RES_MODEL_TUNED_DIR, "categories.csv"), stringsAsFactors = FALSE)

############################################################
##                       TRANSFORM                        ##
############################################################

data_seed$fips_state <- as.factor(data_seed$fips_state)

# add mechanism
model_seed$mechanism <- ""
model_seed[model_seed$classifier %in% c("nb", "lmm", "svm", "neuralnet", "knn", "kknn"), ]$mechanism <- "blackbox"
model_seed[model_seed$classifier %in% c("oner", "jrip", "part", "j48", "cart"), ]$mechanism <- "whitebox"

# add type
model_seed$type <- ""
model_seed[model_seed$exclude == "", ]$type <- "hist"
model_seed[model_seed$exclude == "elec_rep12_win", ]$type <- "geog"
model_seed[model_seed$exclude == "elec_rep12_win, fips_state", ]$type <- "demo"

# reorder the level of each factor
classifiers <- c("oner", "nb", "lmm", "svm", "neuralnet", "jrip", "part", "j48", "cart", "knn", "kknn")
kinds <- c("oner", "nb", "lmm", "svm", "nnet", "jrip", "part", "j48", "cart", "knn", "kknn")
samplings <- c("under", "none", "over")
types <- c("hist", "geog", "demo")

model_seed$type <- factor(model_seed$type, types)
model_seed$sampling <- factor(model_seed$sampling, samplings)
model_seed$classifier <- factor(model_seed$classifier, classifiers)
model_seed$kind <- model_seed$classifier
levels(model_seed$kind) <- kinds

############################################################
##                ENHANCE INTERPRETABILITY                ##
############################################################
# Preprocess the data for better interpretability
data_whitebox <- data_seed
target_whitebox <- "elec_rep16_win"
preprocess_whitebox <- TRUE
if (ENHANCE_INTERPRETABILITY) {
  # Bring all down to percentage range 
  data_whitebox$health_infant_mortality <- data_whitebox$health_infant_mortality/10
  data_whitebox$health_hiv <- data_whitebox$health_hiv/1000
  data_whitebox$health_sti <- data_whitebox$health_sti/1000
  data_whitebox$health_injury <- data_whitebox$health_injury/1000
  data_whitebox$demo_violent_crime <- data_whitebox$demo_violent_crime/1000
  data_whitebox$demo_homicide <- data_whitebox$demo_homicide/1000

  # Rename the Variable
  data_whitebox <- dispd_v(data_seed, meta_seed)
  names(data_whitebox) <- gsub(x = names(data_whitebox),
                        pattern = " ",
                        replacement = "\\_")
  names(data_whitebox) <- gsub(x = names(data_whitebox),
                        pattern = "\\(",
                        replacement = "")
  names(data_whitebox) <- gsub(x = names(data_whitebox),
                        pattern = "\\)",
                        replacement = "")
  names(data_whitebox) <- gsub(x = names(data_whitebox),
                        pattern = ",",
                        replacement = "")
  names(data_whitebox) <- gsub(x = names(data_whitebox),
                        pattern = "-",
                        replacement = "")

  # Special name needs to be addressed
  target_whitebox <- names(data_whitebox)[1]
  names(data_whitebox)[names(data_whitebox) == "Republican_Win_2012"] <- "elec_rep12_win"
  names(data_whitebox)[names(data_whitebox) == "FIPS_State_Code"] <- "fips_state"
  preprocess_whitebox <- FALSE
}

############################################################
##                       VISUALIZE                        ##
############################################################

# +----------------------------------+
# |        COMPARE SAMPLING          |
# +----------------------------------+
# By ACCURACY with SAMPLING over TYPE
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = NULL)
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "sampling_over_type.by_accuracy.png"),
  width = MD_LENGTH,
  height = LG_LENGTH
)
boxplot_quartet(as.simple.formula("sampling", "accuracy"), slices, c(0.8, 1))
dev.off()

# By KAPPA with SAMPLING over TYPE
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = NULL)
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "sampling_over_type.by_kappa.png"),
  width = MD_LENGTH,
  height = LG_LENGTH
)
boxplot_quartet(as.simple.formula("sampling", "kappa"), slices, c(0.3, 0.9))
dev.off()

# By PRECISION.TRUE with SAMPLING over TYPE
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = NULL)
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "sampling_over_type.by_precision.TRUE.png"),
  width = MD_LENGTH,
  height = LG_LENGTH
)
boxplot_quartet(as.simple.formula("sampling", "precision.TRUE"), slices)
dev.off()

# By RECALL.FALSE with SAMPLING over TYPE
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = NULL)
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "sampling_over_type.by_recall.FALSE.png"),
  width = MD_LENGTH,
  height = LG_LENGTH
)
boxplot_quartet(as.simple.formula("sampling", "recall.FALSE"), slices, c(0.5,1))
dev.off()

# +----------------------------------+
# |        COMPARE CLASSIFIER        |
# +----------------------------------+
# By ACCURACY with KIND over TYPE
slices <- slice_by_type_and_sampling(model_seed, types = NULL, samplings = NULL)
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "classifier.by_accuracy.png"),
  width = MD_LENGTH,
  height = LG_LENGTH
)
boxplot_singlet(as.simple.formula("kind", "accuracy"), slices, c(0.5,1))
dev.off()

# By KAPPA with KIND over TYPE
slices <- slice_by_type_and_sampling(model_seed, types = NULL, samplings = NULL)
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "classifier.by_kappa.png"),
  width = MD_LENGTH,
  height = LG_LENGTH
)
boxplot_singlet(as.simple.formula("kind", "kappa"), slices, c(0.3, 0.9))
dev.off()

# By ACCURACY with KIND over TYPE and SAMPLING
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "classifier_over_both.by_accuracy.png"),
  width = XL_LENGTH,
  height = XL_LENGTH
)
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = samplings)
boxplot_nonet(as.simple.formula("kind", "accuracy"), slices)
dev.off()

# By KAPPA with KIND over TYPE and SAMPLING
png(
  mp(RES_MODEL_ANALYZED_PLOT_DIR, "classifier_over_both.by_kappa.png"),
  width = XL_LENGTH,
  height = XL_LENGTH
)
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = samplings)
boxplot_nonet(as.simple.formula("kind", "kappa"), slices, c(0.3, 0.9))
dev.off()

############################################################
##                      MODEL RANK                        ##
############################################################

# slices up the dataset to 9 slices
slices <- slice_by_type_and_sampling(model_seed, types = types, samplings = samplings)
# find modesl with best kappa performance
model_guides <- best_of_each_slice(slices, by = "mechanism", "kappa", base = "oner")
models <- NULL
for (i in 1:length(model_guides)) {
  temp.model <- NULL
  temp.guide <- NULL
  for (j in 1:length(model_guides[[i]])) {
    temp.model[[names(model_guides[[i]])[j]]] <- NULL
    temp.model[[names(model_guides[[i]])[j]]][["blackbox"]] <- retrieve_model(model_guides[[i]][[j]][model_guides[[i]][[j]]$mechanism == "blackbox", ], data_seed, "elec_rep16_win", preprocess = TRUE)
    temp.model[[names(model_guides[[i]])[j]]][["whitebox"]] <- retrieve_model(model_guides[[i]][[j]][model_guides[[i]][[j]]$mechanism == "whitebox", ], data_whitebox, target_whitebox, preprocess = preprocess_whitebox)
    temp.guide <- rbind(temp.guide, model_guides[[i]][[j]])
  }
  models[[names(model_guides)[i]]] <- temp.model
  model_guides[[i]] <- sanitize_result(temp.guide)
}

############################################################
##                        SPECIAL                         ##
############################################################

if (RUN_SPECIAL) {
  # find the best neural networks based on kappa value
  best_nnet <- model_seed[model_seed$classifier == "neuralnet" & model_seed$type == "demo", ]
  # by accuracy
  best_nnet.accuracy <- retrieve_model(best_nnet[which.max(model_seed[model_seed$classifier == "neuralnet", ]$accuracy),], data_seed, "elec_rep16_win")
  png(
    mp(RES_MODEL_ANALYZED_PLOT_DIR, "best_neural_network.by_accuracy.png"),
    width = 2300,
    height = 2300
  )
  par(bg = BACKGROUND_COLOR)
  plot.nn(
    best_nnet.accuracy$model,
    dimension = 28,
    fontsize = 20,
    information = FALSE,
    arrow.length = 0.2,
    show.weights = TRUE,
    col.intercept = COLOR["Black"],
    col.hidden.synapse = COLOR["Black"],
    col.entry = COLOR["Black"],
    col.entry.synapse = COLOR["Black"],
    col.out = COLOR["Black"],
    col.out.synapse = COLOR["Black"],
    bg = BACKGROUND_COLOR
  )
  dev.off()
  # by kappa
  best_nnet.kappa <- retrieve_model(best_nnet[which.max(model_seed[model_seed$classifier == "neuralnet", ]$kappa),], data_seed, "elec_rep16_win")
  png(
    mp(RES_MODEL_ANALYZED_PLOT_DIR, "best_neural_network.by_kappa.png"),
    width = 2300,
    height = 2300
  )
  plot.nn(
    best_nnet.kappa$model,
    dimension = 28,
    fontsize = 20,
    information = FALSE,
    arrow.length = 0.2,
    show.weights = TRUE,
    col.intercept = COLOR["Black"],
    col.hidden.synapse = COLOR["Black"],
    col.entry = COLOR["Black"],
    col.entry.synapse = COLOR["Black"],
    col.out = COLOR["Black"],
    col.out.synapse = COLOR["Black"]
  )
  dev.off()
}

############################################################
##                       CATEGORY                         ##
############################################################

chosen_groups <- category_seed[category_seed$accuracy >= THRESHOLD_ACCURACY_CATEGORY_BASKET & category_seed$kappa >= THRESHOLD_KAPPA_CATEGORY_BASKET, ]
categories <- c("crime", "demographics", "education", "election", "finance", "healthcare", "occupation", "race", "weather")
# create transaction
chosen_groups$group <- ""
for (category in categories) {
  chosen_groups[,category] <- ifelse(chosen_groups[,category], category, "")
  chosen_groups$group <- paste(chosen_groups$group, chosen_groups[,category], sep = " ")
}
groups <- strsplit(chosen_groups$group, " ")
groups <- lapply(groups, function(x){x[!x == ""]})
transactions <- as(groups, "transactions")
# find rules
rules = apriori(transactions, parameter = list(minlen = 2, support = 0.01, confidence = 0.8, target = "rules"), control = list(verbose = FALSE))
rules.confSort <- sort(rules, by = "confidence", decreasing = TRUE)

############################################################
##                        EXPORT                          ##
############################################################

# save computation time for special
if (RUN_SPECIAL) {
  save(best_nnet.kappa, best_nnet.accuracy, file = mp(RES_MODEL_ANALYZED_DIR, "best.neuralnet.RData"))
}
write.csv(
  model_guides$hist,
  file = mp(RES_MODEL_ANALYZED_DIR, "models.hist.csv"),
  row.names = FALSE
)
write.csv(
  model_guides$geog,
  file = mp(RES_MODEL_ANALYZED_DIR, "models.geog.csv"),
  row.names = FALSE
)
write.csv(
  model_guides$demo,
  file = mp(RES_MODEL_ANALYZED_DIR, "models.demo.csv"),
  row.names = FALSE
)
write.csv(
  as(rules.confSort, "data.frame"),
  file = mp(RES_MODEL_ANALYZED_DIR, "categories.rules.csv"),
  row.names = FALSE
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end model analyze script")
reset.work_space()