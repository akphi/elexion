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
               "bookdown",
               "webshot"))
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

html <- function() {
    RENDER_MODE <- "html"
    bookdown::render_book(mp(REPORT_DIR, "index.Rmd"), "bookdown::gitbook")
}

# problem with LaTeX not being able to render image of remote address
# e.g. ![sds](http://127.0.0.1:1923/result/data_analyzed/plots/all_features_scatter.png)
# problem with not being able to generate RAW HTML tage inside a document
pdf <- function() {
    RENDER_MODE <- "pdf"
    bookdown::render_book(mp(REPORT_DIR, "index.Rmd"), "bookdown::pdf_book")
}

build <- function(render_options) {
    if ("html" %in% render_options) {
        html()
    }
    if ("pdf" %in% render_options) {
        pdf()
    }
}