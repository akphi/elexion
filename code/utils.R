############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities methods used for other scripts

############################################################
##                        LIBRARY                         ##
############################################################
# ATTENTION: ORDER MATTERS! ALSO, LOADING DIRECTLY CHILD UTILS
# SHOULD NOT BE ENCOURAGED AS THEY DEPEND ON THIRD-PARTY
# PACKAGES AND THERE WILL BE SCOPE (DIRECTORY) PROBLEMS 

# +----------------------------------+
# |              CONFIG              |
# +----------------------------------+
source("configs.R")

# +----------------------------------+
# |               BASIC              |
# +----------------------------------+
# all smaller utils depend on basic utils
source("utils/basic.R")
source("utils/log.R")

# +----------------------------------+
# |           UTILS LIBRARY          |
# +----------------------------------+
# load all third-party libaries here to prevent
# double-loading them in case more than 2 child
# utils need to share the same packages
install_load(c(
  "FSelector",
  "entropy",
  "plyr",
  "dplyr",
  # classifier
  "caret",
  "RWeka",
  "e1071",
  "arules",                      
  "rpart",
  "class",
  "kknn",
  "neuralnet",
  # plot
  "shiny",
  "ggplot2",
  "scales",
  "grid",
  "maps",
  "arules", 
  "arulesViz",
  "igraph",
  "visNetwork"
  )
)

# +----------------------------------+
# |             GENERAL              |
# +----------------------------------+
source("utils/export.R")
source("utils/meta.R")
source("utils/exploration.R")

# +----------------------------------+
# |           MANIPULATION           |
# +----------------------------------+
source("utils/transform.R")
source("utils/missing.R")
source("utils/feature_selection.R")
source("utils/tuner.R")
source("utils/analyze.R")

# +----------------------------------+
# |          VISUALIZATION           |
# +----------------------------------+
source("utils/plot.R")

# +----------------------------------+
# |               APP                |
# +----------------------------------+
source("utils/app.R")
source("utils/app_support.R")

# +----------------------------------+
# |             DOCUMENT             |
# +----------------------------------+
source("utils/markdown.R")