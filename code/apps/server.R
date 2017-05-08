############################################################
##                      DESCRIPTION                       ##
############################################################

# apps that create servers to serve documents, presentations
# etc.

############################################################
##                       APPLICATION                      ##
############################################################

# create server for reading the report
server.resource <- makeApp(PORTS$RESOURCE, function(port) {
  library("servr")
  httd(dir = BASE_DIR, port = port)
})

# create server for viewing the keynote
server.keynote <- makeApp(PORTS$KEYNOTE, function(port) {
  library("servr")
  httd(dir = KEYNOTE_DIR, port = port)
})

# create server for reading the report
server.report <- makeApp(PORTS$REPORT, function(port) {
  library("servr")
  httd(dir = DOCUMENT_DIR, port = port)
})

# create server for viewing the presentation
server.presentation <- makeApp(PORTS$PRESENTATION, function(port) {
  library("servr")
  httd(dir = PRESENTATION_DIR, port = port)
})
