############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities methods used for report

############################################################
##                        LIBRARY                         ##
############################################################

setwd("../")
source("utils.R")
setwd("./report")
source("configs.R")
install_load(
  c(
    "htmltools",
    "xtable",
    "DT"
  )
)

############################################################
##                        METHOD                          ##
############################################################

# create path for content based on host
mp_host <- function(path, port = PORTS$RESOURCE, host = HOSTS$LOCAL, base = BASE_DIR) {
  path <- gsub(paste(base, "/", sep = ""), "", path)
  return(paste(paste(host, ":", port, sep = ""), path, sep = "/"))
}

# generate image base on link
generate_image <- function(caption, img_url) {
  result <- NULL
  if (RENDER_MODE == "pdf") {
    result <- paste("\\greybox{[", caption, "] LaTeX does not support viewing remotely located images, please click \\href{", img_url, "}{here} to view it.}", sep = "")
    return(cat(result))
  } else {
    # result <- paste("![", caption, "](", , ")", sep = "")
    knitr::include_graphics(img_url)
  }
}