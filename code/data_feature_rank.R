############################################################
##                      DESCRIPTION                       ##
############################################################

# prepare the dataset by selecting features

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
source("misc.R")
install_load(c("FSelector",
               "rpart", 
               "doParallel"))
registerDoParallel(NUMBER_OF_CORE_PARALLEL)

############################################################
##                      INITIALIZE                        ##
############################################################

log.write("start data_feature_rank script")
log.write(
  "running scheme-independent selection with ",
  FEATURE_RANKING_SEARCH_CROSS_VALIDATION_FOLD,
  "-fold(s) cross validation and ",
  FEATURE_RANKING_SEARCH_ITERATION,
  " iteration(s) for attribute space search"
)

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
  read.csv(mp(DATA_PREPARED_DIR, "data.csv"), stringsAsFactors = TRUE)
meta_orig <-
  read.csv(mp(DATA_PREPARED_DIR, "meta.csv"), stringsAsFactors = FALSE)
# set MAIN
data_seed <- data_orig
meta_seed <- meta_orig

############################################################
##                      TRANSFORM                         ##
############################################################

data_seed$fips_state <- as.factor(data_seed$fips_state)

############################################################
##     FEATURE-SELECTION: SCHEME-INDEPENDENT SELECTION    ##
############################################################

# @FS001
# +----------------------------------+
# |     RANDOM FOREST IMPORTANCE     |
# +----------------------------------+
print(paste(Sys.time(), "FS: random forest", sep = "    "))
result_random_forest_importance <-
  random.forest.importance(elec_rep16_win ~ ., data_seed)
result_random_forest_importance$name <-
  rownames(result_random_forest_importance)
result_random_forest_importance <-
  result_random_forest_importance[order(abs(result_random_forest_importance$attr_importance),
                                        decreasing = TRUE), ]
result_random_forest_importance <- data.frame(
  name = rownames(result_random_forest_importance),
  importance = result_random_forest_importance$attr_importance
)

# @FS002
# +----------------------------------+
# |               CFS                |
# +----------------------------------+
print(paste(Sys.time(), "FS: using correlation and entropy", sep = "    "))
result_cfs <- exhaustive.cfs(data_seed, "elec_rep16_win")

# @FS003
# +----------------------------------+
# | LINEAR MODEL COEFFICIENT RANKING |
# +----------------------------------+
# leave out nominal and standardize all numerical features
print(paste(Sys.time(), "FS: linear model coefficient ranking", sep = "    "))
data_seed_lm <-
  data_seed[, names(data_seed) %notin% c("fips_state")]
data_seed_lm <-
  transform.numeric_data(data_seed_lm, FUN = normalize, na.rm = TRUE)
data_seed_lm$elec_rep16_win <-
  as.numeric(data_seed_lm$elec_rep16_win)
data_seed_lm$elec_rep12_win <-
  as.numeric(data_seed_lm$elec_rep12_win)
table_lm_coef_ranking.smallest_first <-
  data.frame(name = exhaustive.linear_model.by_coefficient(data_seed_lm, "elec_rep16_win"))
table_lm_coef_ranking.smallest_first$remove_smallest_first <- seq(nrow(table_lm_coef_ranking.smallest_first),1, -1)
table_lm_coef_ranking.biggest_first <-
  data.frame(name = exhaustive.linear_model.by_coefficient(data_seed_lm,
                                                           "elec_rep16_win", remove.smallest.first = FALSE))
table_lm_coef_ranking.biggest_first$remove_biggest_first <- seq(nrow(table_lm_coef_ranking.biggest_first),1, -1)
result_lm_coef <- merge(table_lm_coef_ranking.biggest_first, table_lm_coef_ranking.smallest_first, by = "name")

############################################################
##       ATTRIBUTE SPACE SEARCH: LINEAR REGRESSION        ##
############################################################
# convert the target into factor so it can be used with confusionMatrix()
data_as.lm <- data_seed[, names(data_seed) %notin% c("fips_state")]
data_as.lm <- transform.numeric_data(data_as.lm, FUN = normalize, na.rm = TRUE)
data_as.lm$elec_rep12_win <- as.numeric(data_as.lm$elec_rep12_win)
data_as.lm[, "elec_rep16_win"] <- as.factor(data_as.lm[, "elec_rep16_win"])

# @FS004
# +----------------------------------+
# |          SCHEME 0: FULL          |
# +----------------------------------+
print(paste(Sys.time(), "FS: attribute space search using Linear Regression (full)", sep = "    "))
result_lm_eval.accuracy.full <-
  fs.feature_select(
    data_as.lm,
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "linear",
      metric = "accuracy"
    ),
    sampling = "under",
    verbose = FALSE
  )
result_lm_eval.kappa.full <-
  fs.feature_select(
    data_as.lm,
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "linear",
      metric = "kappa"
    ),
    sampling = "under",
    verbose = FALSE
  )

# +----------------------------------+
# | SCHEME 1: EXCLUDE elec_rep12_win |
# +----------------------------------+
print(paste(Sys.time(), "FS: attribute space search using Linear Regression (exclude elec_rep12_win)", sep = "    "))
result_lm_eval.accuracy.1 <-
  fs.feature_select(
    getrod(data_as.lm, c("elec_rep12_win")),
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "linear",
      metric = "accuracy"
    ),
    sampling = "under",
    verbose = FALSE
  )
result_lm_eval.kappa.1 <-
  fs.feature_select(
    getrod(data_as.lm, c("elec_rep12_win")),
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "linear",
      metric = "kappa"
    ),
    sampling = "under",
    verbose = FALSE
  )

############################################################
##          ATTRIBUTE SPACE SEARCH: DECISION-TREE         ##
############################################################
# convert the target into factor so it can be used with confusionMatrix()
data_as <- data_seed
data_as[, "elec_rep16_win"] <- as.factor(data_as[, "elec_rep16_win"])

# @FS005
# +----------------------------------+
# |          SCHEME 0: FULL          |
# +----------------------------------+
print(paste(Sys.time(), "FS: attribute space search using CART (full)", sep = "    "))
result_tree_eval.accuracy.full <-
  fs.feature_select(
    data_as,
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "tree",
      metric = "accuracy"
    ),
    sampling = "under"
  )
result_tree_eval.kappa.full <-
  fs.feature_select(
    data_as,
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "tree",
      metric = "kappa"
    ),
    sampling = "under"
  )

# +----------------------------------+
# | SCHEME 1: EXCLUDE elec_rep12_win |
# +----------------------------------+
print(paste(Sys.time(), "FS: attribute space search using CART (exclude elec_rep12_win)", sep = "    "))
result_tree_eval.accuracy.1 <-
  fs.feature_select(
    getrod(data_as, c("elec_rep12_win")),
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "tree",
      metric = "accuracy"
    ),
    sampling = "under"
  )
result_tree_eval.kappa.1 <-
  fs.feature_select(
    getrod(data_as, c("elec_rep12_win")),
    "elec_rep16_win",
    eval.opts = list(
      target_type = "nominal",
      model = "tree",
      metric = "kappa"
    ),
    sampling = "under"
  )

############################################################
##                        EXPORT                          ##
############################################################

write.csv(
  result_random_forest_importance,
  file = mp(RES_FEATURE_RANKED_DIR, "rfi.csv"),
  row.names = FALSE
)
write.csv(
  result_lm_coef,
  file = mp(RES_FEATURE_RANKED_DIR, "lm_coef.csv"),
  row.names = FALSE
)
write.csv(
  result_cfs,
  file = mp(RES_FEATURE_RANKED_DIR, "cfs.csv"),
  row.names = FALSE
)
export.evaluator.result(
  list(result_lm_eval.accuracy.full,
      result_lm_eval.kappa.full,
      result_lm_eval.accuracy.1,
      result_lm_eval.kappa.1),
  "as.lm",
  RES_FEATURE_RANKED_DIR,
  c("accuracy.full",
    "kappa.full",
    "accuracy.1",
    "kappa.1"),
  RES_FEATURE_RANKED_FORMULA_DIR
)
export.evaluator.result(
  list(
    result_tree_eval.accuracy.full,
    result_tree_eval.kappa.full,
    result_tree_eval.accuracy.1,
    result_tree_eval.kappa.1
  ),
  "as.tree",
  RES_FEATURE_RANKED_DIR,
  c("accuracy.full",
    "kappa.full",
    "accuracy.1",
    "kappa.1"),
  RES_FEATURE_RANKED_FORMULA_DIR
)

############################################################
##                       TERMINATE                        ##
############################################################

log.write("end data_feature_rank script")
# stop parallel cores
stopImplicitCluster()
reset.work_space()