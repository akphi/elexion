############################################################
##                      DESCRIPTION                       ##
############################################################

# apps that shows the visNetwork for association rules

############################################################
##                       APPLICATION                      ##
############################################################

source("utils.R")
category.association.by_lift <- association.category.by_metric(
  read.csv(mp(RES_MODEL_TUNED_DIR, "categories.csv"), stringsAsFactors = FALSE),
  threshold.accuracy = 0.85,
  threshold.kappa = 0.6, 
  by = "lift",
  name = "category_association.top_lift.png"
)
runApp(category.association.by_lift, port = PORTS$VISNETWORK_BY_LIFT, quiet = TRUE)