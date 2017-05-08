############################################################
##                      DESCRIPTION                       ##
############################################################

# configs for the scripts: directories, options, etc.

############################################################
##                        LIBRARY                         ##
############################################################

source("../configs.R")

############################################################
##                       DIRECTORY                        ##
############################################################

BASE_DIR <- "../.."

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
DOCUMENT_DIR <- paste(BASE_DIR, "document", sep = "/")

BACKUP_DIR <- paste(BASE_DIR, "backup", sep = "/")
LOG_FILE <- paste(BASE_DIR, "script.log", sep = "/")

############################################################
##                       CONSTANT                         ##
############################################################

############################################################
##                       DISPLAY                          ##
############################################################
