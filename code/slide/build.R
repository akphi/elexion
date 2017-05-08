############################################################
##                      DESCRIPTION                       ##
############################################################

# script of miscellaneous tasks

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
install_load(c("rmarkdown",
               "revealjs",
               "webshot"))
# Might need to install via source
# install.packages("revealjs", type = "source")

# webshot::install_phantomjs()

# https://bookdown.org/yihui/bookdown/build-the-book.html
# https://bookdown.org/yihui/bookdown/html-widgets.html
# we need to use webshot to takes screenshot of html widgets
# that PDF format cannot handle. But webshot depeneds on 
# phantomjs so we need to instlal phantomjs

# we also need to run the server for server-rendered content
# as we publish the pdf

############################################################
##                         TASK                           ##
############################################################

build <- function() {
    RENDER_MODE <- "html"
    rmarkdown::render(mp(SLIDE_DIR, "index.Rmd"), output_dir = PRESENTATION_DIR)
}