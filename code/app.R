############################################################
##                      DESCRIPTION                       ##
############################################################

# script of miscellaneous tasks

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
install_load(c(
  "shiny",
  "servr",
  "parallel",
  # plot
  "ggplot2",
  "Cairo",
  "maps"
  )
)

############################################################
##                       APPLICATION                      ##
############################################################

source("apps/datatable.R")
source("apps/image.R")
source("apps/plot.R")
source("apps/choropeth.R")
source("apps/server.R")

plot.correlation <- image.zoom(
  PORTS$GIANT_CORRELATION_PLOT,
  mp(RES_ANALYZED_PLOT_DIR, "all_features_correlation.png"),
  width = 500,
  height = 500,
  'zoomWindowPosition: 1, zoomWindowHeight: 500, zoomWindowWidth: 220, borderSize: 3, cursor: "pointer"'
)

choropeth.pre_processed <- choropeth.by_county(
  PORTS$CHOROPETH_PRE_PROCESSED,
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "data.csv"), stringsAsFactors = FALSE),
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "meta.csv"), stringsAsFactors = FALSE),
  exclude <- c("name_16")
)

choropeth.with_preset <- choropeth.by_county.with_preset(
  PORTS$CHOROPETH_WITH_PRESET,
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "data.csv"), stringsAsFactors = FALSE),
  read.csv(mp(DATA_PRE_PROCESSED_DIR, "meta.csv"), stringsAsFactors = FALSE),
  exclude <- c("name_16")
)

data_prepared.with_fips <- read.csv(mp(DATA_PREPARED_DIR, "data.csv"), stringsAsFactors = FALSE)
colnames(data_prepared.with_fips) <- namev_v(names(data_prepared.with_fips), 
  read.csv(mp(DATA_PREPARED_DIR, "meta.csv"), stringsAsFactors = FALSE))
fips <- read.csv(mp(DATA_PRE_PROCESSED_DIR, "data.csv"), stringsAsFactors = FALSE)$fips
# take away the fips of unreliable county
data_prepared.with_fips$fips <- fips[fips != 53075]
choropeth.prepared <- choropeth.by_county(
  PORTS$CHOROPETH_PREPARED,
  data_prepared.with_fips,
  read.csv(mp(DATA_PREPARED_DIR, "meta.csv"), stringsAsFactors = FALSE),
  exclude <- c("name_16", "fips")
)

############################################################
##                          RUNNER                        ##
############################################################
apps <- list(
  server.keynote,
  server.resource,
  server.report,
  server.presentation,
  plot.correlation,
  data.pre_processed,
  data.selected,
  choropeth.pre_processed,
  choropeth.prepared,
  choropeth.with_preset
)

# run multiple R application on different ports
# https://statisticalplumber.wordpress.com/2014/04/21/run-two-or-more-than-two-shiny-applications-from-r-gui/
serveApp <- function(verbose = SERVER_VERBOSE) {
  # info section
  if (verbose) {
    writeLines(horizontal_rule(" "))
    writeLines(horizontal_rule())
    writeLines(horizontal_rule(" "))
    writeLines("The app has started, your browser can navigate them now at localhost with the ports assigned in configs.R. For example, the following ports worth a visit:")
    writeLines(horizontal_rule(" "))
    writeLines(paste("         1. KEYNOTE:                 localhost:", PORTS$KEYNOTE, sep = ""))
    writeLines(paste("         2. REPORT:                  localhost:", PORTS$REPORT, sep = ""))
    writeLines(paste("         3. VISUALIZATION SUPPORT:   localhost:", PORTS$PRESENTATION, sep = ""))
    writeLines("         ...")
    writeLines(horizontal_rule(" "))
    writeLines("However, do not navigate to these sites while the document building process is happening because the browser used for nagivation will be killed after the documents have been built!")
    writeLines(horizontal_rule(" "))
    writeLines(horizontal_rule())
    writeLines(horizontal_rule(" "))
  }
  # start the cluster
  cluster <- makeCluster(length(apps))
  out <- clusterMap(cluster, function(x, y) {
    source("configs.R")
    source("utils/basic.R")
    source("utils/app_support.R")
    (x$app)(port = x$port)
  }, x = apps)
  stopCluster(cluster)
}
