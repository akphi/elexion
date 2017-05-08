############################################################
##                      DESCRIPTION                       ##
############################################################

# configs for the scripts: directories, options, etc.

############################################################
##                       DIRECTORY                        ##
############################################################

BASE_DIR <- ".."

DATA_DIR <- paste(BASE_DIR, "data", sep = "/")
DATA_MISC_DIR <- paste(DATA_DIR, "misc", sep = "/")
DATA_RAW_DIR <- paste(DATA_DIR, "raw", sep = "/")
DATA_ORIGINAL_DIR <- paste(DATA_DIR, "original", sep = "/")
DATA_PRE_PROCESSED_DIR <- paste(DATA_DIR, "pre_processed", sep = "/")
DATA_PREPARED_DIR <- paste(DATA_DIR, "prepared", sep = "/")
DATA_SELECTED_DIR <- paste(DATA_DIR, "selected", sep = "/")

RES_DIR <- paste(BASE_DIR, "result", sep = "/")
RES_PRE_PROCESSED_DIR <- paste(RES_DIR, "data_pre_processed", sep = "/")
RES_ANALYZED_DIR <- paste(RES_DIR, "data_analyzed", sep = "/")
RES_ANALYZED_PLOT_DIR <- paste(RES_ANALYZED_DIR, "plots", sep = "/")
RES_ANALYZED_HISTORGRAM_DIR <- paste(RES_ANALYZED_DIR, "histograms", sep = "/")
RES_PREPARED_DIR <- paste(RES_DIR, "data_prepared", sep = "/")
RES_FEATURE_RANKED_DIR <- paste(RES_DIR, "data_feature_ranked", sep = "/")
RES_FEATURE_RANKED_FORMULA_DIR <- paste(RES_FEATURE_RANKED_DIR, "formula", sep = "/")
RES_SELECTED_DIR <- paste(RES_DIR, "data_selected", sep = "/")
RES_SELECTED_PLOT_DIR <- paste(RES_SELECTED_DIR, "plots", sep = "/")
RES_MODEL_PROBED_DIR <- paste(RES_DIR, "model_probed", sep = "/")
RES_MODEL_PROBED_PLOT_DIR <- paste(RES_MODEL_PROBED_DIR, "plots", sep = "/")
RES_MODEL_TUNED_DIR <- paste(RES_DIR, "model_tuned", sep = "/")
RES_MODEL_ANALYZED_DIR <- paste(RES_DIR, "model_analyzed", sep = "/")
RES_MODEL_ANALYZED_PLOT_DIR <- paste(RES_MODEL_ANALYZED_DIR, "plots", sep = "/")

CODE_DIR <- paste(BASE_DIR, "code", sep = "/")
REPORT_DIR <- paste(CODE_DIR, "report", sep = "/")
REPORT_STYLE <- paste(REPORT_DIR, "style.css", sep = "/")
DOCUMENT_DIR <- paste(BASE_DIR, "document", sep = "/")
SLIDE_DIR <- paste(CODE_DIR, "slide", sep = "/")
PRESENTATION_DIR <- paste(BASE_DIR, "presentation", sep = "/")
KEYNOTE_DIR <- paste(BASE_DIR, "keynote", sep = "/")

BACKUP_DIR <- paste(BASE_DIR, "backup", sep = "/")
LOG_FILE <- paste(BASE_DIR, "script.log", sep = "/")

############################################################
##                      PERFORMANCE                       ##
############################################################

REPRODUCIBILITY <- TRUE
ENABLE_MODEL_PARAMETER_PROBING <- TRUE
NUMBER_OF_CORE_PARALLEL <- 4
RNG_KIND <- "L'Ecuyer-CMRG"
RNG_SEED <- 0
# WARNING: Please do not change this number abitrarily small
# without understanding the consequence of your action
# this is the number of seconds delayed between the server built
# and the report being rendered, if the delay is not long enough
# and the server is not ready at the render time of the document
# error will occur. of course, don't set it too big neither.
TIME_INTERVAL_DELAY_DOCUMENT_BUILD <- 10

############################################################
##                         SERVER                         ##
############################################################

HOSTS <- list(
  LOCAL = "http://127.0.0.1"
)
# keep this in a list so we can easily manage easier
PORTS <- list(
  RESOURCE = 1923,
  KEYNOTE = 2301,
  REPORT = 2302,
  PRESENTATION = 2303,
  GIANT_CORRELATION_PLOT = 2304,
  DATA_PRE_PROCESSED = 2305,
  DATA_SELECTED = 2306,
  PCA_PLOT = 2307,
  CHOROPETH_PRE_PROCESSED = 2308,
  CHOROPETH_PREPARED = 2309,
  VISNETWORK_BY_SUPPORT = 2310,
  VISNETWORK_BY_CONFIDENCE = 2311,
  VISNETWORK_BY_LIFT = 2312,
  CHOROPETH_WITH_PRESET = 2313,
  VISNETWORK_PCA = 2314
)

############################################################
##                       SETTINGS                         ##
############################################################

# control infos about library loading, masking of functions and conflicts 
LOADER_VERBOSE <- FALSE
LOADER_CRAN_GRAPHIC_ENABLED <- FALSE
# choose the CRAN mirror so Script can be loaded silently
# 49: USA (CA 1) [https]             50: USA (IA) [https]
# 51: USA (IN) [https]               52: USA (KS) [https]
# 53: USA (MI 1) [https]             54: USA (OR) [https]
# 55: USA (TN) [https]               56: USA (TX 1) [https]
# 57: USA (TX 2) [https]             58: (HTTP mirrors)
LOADER_CRAN_MIRROR <- 51
# control the message shown during server startup
SERVER_VERBOSE <- TRUE
SERVER_KILL_VERBOSE <- TRUE
DOCUMENT_RENDER_REMOVE_CACHE <- TRUE
DOCUMENT_RENDER_OUTPUT <- c("html")
FEATURE_RANKING_SEARCH_CROSS_VALIDATION_FOLD <- 5
FEATURE_RANKING_SEARCH_ITERATION <- 5
NEURAL_NET_STEPMAX <- 1e+07

############################################################
##                       DISPLAY                          ##
############################################################

BACKGROUND_COLOR <- "#FFFFFF"
# BACKGROUND_COLOR <- NA # transparent
# BACKGROUND_COLOR <- "#fdf6e3" # for presentation
XS_LENGTH <- 300
SM_LENGTH <- 500
MD_LENGTH <- 700
LG_LENGTH <- 900
XL_LENGTH <- 1100
XXL_LENGTH <- 1300
MS_LENGTH <- 1500

COLOR <- c(
  White = "#ffffff",
  "Dem. Blue" = "#232066",
  "Rep. Red" = "#E91D0E",
  Navy = "#001f3f",
  Blue = "#0074D9",
  Aqua = "#7FDBFF",
  Teal = "#39CCCC",
  Olive = "#3D9970",
  Green = "#2ECC40",
  Lime = "#01FF70",
  Yellow = "#FFDC00",
  Orange = "#FF851B",
  Red = "#FF4136",
  Maroon = "#85144b",
  Fuchsia = "#F012BE",
  Purple = "#B10DC9",
  Silver = "#DDDDDD",
  "Light Grey" = "#f1f1f1",
  Gray = "#AAAAAA",
  "Dark Grey" = "#2d2d2d",
  Black = "#111111"
)
