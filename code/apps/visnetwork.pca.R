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
install_load(c(
          "corrplot",
          "igraph",
          "ggbiplot",
          "visNetwork"))

############################################################
##                      INITIALIZE                        ##
############################################################

# allow the same result as in the paper by setting the seed
if (REPRODUCIBILITY) {
  set.seed(RNG_SEED)
}
# setting weights for the feature scoring system
feature_scoring_weights = c(
  result_cfs = 1,
  result_lm_coef = 0.5,
  result_rfi = 1,
  result_as.lm = 0.25,
  result_as.tree = 0.25
)
# threshold below which the edge will not be drawn in the graph that is used
# to search for cluster of correlated features
CORRELATION_CUTOFF_THRESHOLD <- 0.65
MIN_VERTEX_DEGREE <- 2
# minimum portion of variances required to be covered by the selected PCs
PCA_VARIANCE_COVERAGE_THRESHOLD <- 0.9 # 90%
# amount of tolerance allowed to search around the threshold 
PCA_VARIANCE_COVERAGE_TOLERANCE <- 0.02 # 2%

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
##                     FEATURE SCORE                      ##
############################################################

# random forest importance
result_rfi <-
  read.csv(mp(RES_FEATURE_RANKED_DIR, "rfi.csv"),
           stringsAsFactors = FALSE)
colnames(result_rfi) <- c("name", "rfi")
result_rfi$rfi <- abs(result_rfi$rfi)
# linear model coefficient
result_lm_coef <- read.csv(mp(RES_FEATURE_RANKED_DIR, "lm_coef.csv"),
           stringsAsFactors = FALSE)
colnames(result_lm_coef) <- c("name", "lm_coef.remove_biggest_first", "lm_coef.remove_smallest_first")
# cfs
result_cfs <-
  read.csv(mp(RES_FEATURE_RANKED_DIR, "cfs.csv"), stringsAsFactors = FALSE)
colnames(result_cfs) <- c("name", "cfs")
result_cfs$cfs <- nrow(result_cfs) - result_cfs$cfs
# attribute space search: tree and linear model evaluator
result_as.lm <-
  read.csv(mp(RES_FEATURE_RANKED_DIR, "as.lm.csv"),
           stringsAsFactors = FALSE)
colnames(result_as.lm) <- c("name", "as.lm.accuracy.full", "as.lm.kappa.full", "as.lm.accuracy.1", "as.lm.kappa.1")
result_as.tree <-
  read.csv(mp(RES_FEATURE_RANKED_DIR, "as.tree.csv"),
           stringsAsFactors = FALSE)
colnames(result_as.tree) <- c("name", "as.tree.accuracy.full", "as.tree.kappa.full", "as.tree.accuracy.1", "as.tree.kappa.1")

result_cfs <-
  feature_rank.by_importance(result_cfs, ncol(data_seed))
result_lm_coef <-
  feature_rank.by_importance(result_lm_coef, ncol(data_seed))
result_rfi <-
  feature_rank.by_importance(result_rfi, ncol(data_seed))
result_as.lm <-
  feature_rank.by_importance(result_as.lm, ncol(data_seed))
result_as.tree <-
  feature_rank.by_importance(result_as.tree, ncol(data_seed))

result_feature_rank <- feature_rank.compile(
  data.frame(name = names(getrod(
    data_seed, c("elec_rep16_win")
  ))),
  list(result_cfs,
      result_lm_coef,
      result_rfi,
      result_as.lm,
      result_as.tree),
  feature_scoring_weights
)

result_feature_rank$name <-
  dispv_v(result_feature_rank$name, meta_seed)

############################################################
##                       CORRELATION                      ##
############################################################

# +----------------------------------+
# |        CORRELATION MATRIX        |
# +----------------------------------+
data_cor <- getrod(data_seed, c("fips_state"))
data_cor$elec_rep16_win <- as.numeric(data_cor$elec_rep16_win)
data_cor$elec_rep12_win <- as.numeric(data_cor$elec_rep12_win)
data_cor <-
  compute.correlation.pair_wise(data_cor, sort = TRUE, decreasing = TRUE)
data_cor <- data.frame(
  feature.1 = dispv_v(data_cor$x, meta_seed),
  feature.2 = dispv_v(data_cor$y, meta_seed),
  correlation = data_cor$cor,
  p_value = data_cor$p
)

# +----------------------------------+
# |      CORRELATION WITH SCORE      |
# +----------------------------------+
data_cor_matrix <- abs(cor(data_seed[-2]))
data_score <- result_feature_rank[, c("name", "score")]
data_score$name <-
  sapply(data_score$name, function(x)
    return(var_d(x, meta_seed)))
plot.cor <- graph.adjacency(data_cor_matrix, weighted = TRUE, mode = "undirected")
# simplify the graph by removing self-loops, multi-edge, edges with weights below threshold
plot.cor <- simplify(plot.cor)
plot.cor <- delete.edges(plot.cor, which(E(plot.cor)$weight <= CORRELATION_CUTOFF_THRESHOLD))
plot.cor <- delete.vertices(plot.cor, which(degree(plot.cor) < 1))
# make the color scale of the vertices reflect the score in feature-rank
c_scale <- colorRamp(c("#ffff99", "#cc0000"))
vertex_importance <-
  sapply(V(plot.cor)$name, function(x)
    return(data_score[data_score$name == x,]$score))
vertex_importance[[1]] <- NA
vertex_importance <- as.vector(unlist(vertex_importance))
vertex_importance[is.na(vertex_importance)] <-
  max(vertex_importance, na.rm = TRUE)
V(plot.cor)$color <-
  apply(c_scale(normalize(vertex_importance)), 1, function(x)
    rgb(x[1] / 255, x[2] / 255, x[3] / 255))
V(plot.cor)$label.cex <- 3.2
E(plot.cor)$width <- 2
V(plot.cor)$frame.color <- "#666666"
png(
  mp(RES_SELECTED_PLOT_DIR, "scored_features_correlation_network.png"),
  width = 3000,
  height = 2300
)
ig <- plot(
  plot.cor,
  vertex.label = ifelse(degree(plot.cor) >= MIN_VERTEX_DEGREE, c(1:length(V(plot.cor)$name)), rep(NA, length(V(plot.cor)$name))),
  vertex.label.color = "black",
  # how big a vertex is reflects its degree, in other words,
  # the more correlating it is to other vertices
  vertex.size = (2 + degree(plot.cor)) * 1.3,
  layout = layout.fruchterman.reingold(plot.cor),
  xlim = c(-1, 1.65),
  ylim = c(-0.52, 0.65)
)
legend(
  'bottomright',
  inset = c(0.02,0),
  legend = paste(c(1:length(V(plot.cor)$name)), trim_strings(dispv_v(V(plot.cor)$name, meta_seed), 
    30, elip = TRUE), sep = ". "),
  cex = 2.4,
  bty = "o",
  fill = as.vector(apply(c_scale(normalize(vertex_importance)), 1, function(x)
    rgb(x[1] / 255, x[2] / 255, x[3] / 255)))
)
dev.off()

############################################################
##                          PCA                           ##
############################################################

# @FS006
# +----------------------------------+
# |               PCA                |
# +----------------------------------+
# breadth-first search to find the low-scoring features cluster
low_scoring <- data_score[data_score$score <= (max(data_score$score, na.rm = TRUE) + min(data_score$score, na.rm = TRUE))/2 & !is.na(data_score$score),]$name
vname <- names(V(plot.cor))[names(V(plot.cor)) %in% low_scoring]
vdeg <- degree(plot.cor)[names(degree(plot.cor)) %in% low_scoring]
# get rid of all vertices with degree 1
vname <- vname[vdeg > 1]
pca_chosen_features <- NULL
current <- NULL
queue <- list()
# find the starting vertex (highest degree)
start <- vname[which.max(degree(plot.cor)[names(degree(plot.cor)) %in% low_scoring])]
queue[[length(queue)+1]] <- start
vname <- vname[vname != start]
while(length(queue) > 0) {
  current <- queue[[1]]
  queue <- queue[-1]
  pca_chosen_features <- append(pca_chosen_features, current)
  neighbors <- neighbors(plot.cor, current)$name[neighbors(plot.cor, current)$name %in% vname]
  for (i in neighbors) {
    queue[[length(queue)+1]] <- i
    vname <- vname[vname != i]
  }
}
pca_orig_features <- data_seed[,pca_chosen_features]
# principal component analysis on the covariance of chosen features
pca.obj <- prcomp(pca_orig_features, center = TRUE, scale. = TRUE)
pca_rotation <- pca.obj$rotation


pca_app <- shinyApp(
  ui = fluidPage(
    column(12, align="center",
      visNetworkOutput("network", width = "100%", height = "500px")
    )
  ),
  server = function(input, output) {

    library(shiny)
    library(igraph)
    library(visNetwork)

    # output
    output$network <- renderVisNetwork({
      ig_df <- get.data.frame(plot.cor, what = "both" )
      visNetwork(
        nodes = data.frame(
          id = ig_df$vertices$name
          # ,title = dispv_v(ig_df$vertices$name, meta_seed)
          ,label = dispv_v(ig_df$vertices$name, meta_seed)
          ,ig_df$vertices
        ),
        edges = data.frame(
          from = ig_df$edges$from
          ,to = ig_df$edges$to
          ,value = ig_df$edges$weight/2
          ,width = 0.5
        ),
        width = 1000,
        height = 1000
      ) %>% visOptions( highlightNearest = TRUE, autoResize = FALSE, height = "600px", width = "600px") %>% visEdges( arrows = "none" ) %>% visNodes( shape = "box", font = list(size = 20)) %>% visLegend()
    })
  },
  options = list(launch.browser = FALSE)
)

runApp(pca_app, port = PORTS$VISNETWORK_PCA, quiet = TRUE)