############################################################
##                      DESCRIPTION                       ##
############################################################

# run all the script automatically in a defined order

############################################################
##                      INITIALIZE                        ##
############################################################

source("misc.R")
log.write()
log.write("START AUTO SCRIPT")
mkdirs()
reset.results()
reset.work_space()

############################################################
##                          RUN                           ##
############################################################

source("misc.R")
if (REPRODUCIBILITY) {
  log.write("REPRODUCIBILITY enabled: fixing random seed")
}
# preparing data
source("data_pre_process.R")
source("data_prepare.R")
source("data_analyze.R")
source("data_feature_rank.R")
source("data_select.R")
# modelling and evaluation
# this may take a long time
source("misc.R")
if (ENABLE_MODEL_PARAMETER_PROBING) {
  source("model_probe.R")
}
source("model_tune.R")
source("model_analyze.R")
# build the report and the presentation
source("misc.R")
build_documents(TRUE)
source("misc.R")
build_presentation(TRUE)

############################################################
##                      TERMINATE                         ##
############################################################

source("misc.R")
log.write("END AUTO SCRIPT")
log.write()
reset.work_space()