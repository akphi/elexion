############################################################
##                      DESCRIPTION                       ##
############################################################

# apps that shows data tables

############################################################
##                       APPLICATION                      ##
############################################################

# data pre processed
data.pre_processed <- makeShinyApp(PORTS$DATA_PRE_PROCESSED, shinyApp(
  ui = fluidPage(DT::dataTableOutput('tbl'),
                 includeCSS(REPORT_STYLE)),
  server = function(input, output) {
    source("utils.R")
    data <- read.csv(mp(DATA_PRE_PROCESSED_DIR, "data.csv"), stringsAsFactors = TRUE)
    data <- dispd(data, read.csv(mp(DATA_PRE_PROCESSED_DIR, "meta.csv"), stringsAsFactors = FALSE))
    output$tbl = DT::renderDataTable(data,
                                     options = list(
                                       scrollX = TRUE,
                                       scrollY = TRUE,
                                       scrollCollapse = TRUE
                                     ))
  },
  options = list(launch.browser = FALSE)
))

# ABT
data.selected <- makeShinyApp(PORTS$DATA_SELECTED, shinyApp(
  ui = fluidPage(DT::dataTableOutput('tbl'),
                 includeCSS(REPORT_STYLE)),
  server = function(input, output) {
    source("utils.R")
    data <- read.csv(mp(DATA_SELECTED_DIR, "data.csv"), stringsAsFactors = TRUE)
    data <- dispd_v(data, read.csv(mp(DATA_SELECTED_DIR, "meta.csv"), stringsAsFactors = FALSE))
    output$tbl = DT::renderDataTable(data,
                                     options = list(
                                       scrollX = TRUE,
                                       scrollY = TRUE,
                                       scrollCollapse = TRUE
                                     ))
  },
  options = list(launch.browser = FALSE)
))