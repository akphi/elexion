############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities for making apps

############################################################
##                        METHOD                          ##
############################################################

# create app to put into cluster map
makeApp <- function(port = NULL, FUN = function(port) { print(port) }) {
  result <- NULL
  result$app <- FUN
  result$port <- port
  return(result)
}

# create a funciton that runs shiny object
shinyWrapper <- function(shinyApp, launch.browser) {
  return(function(port) {
    library(shiny)
    runApp(shinyApp, port = port, launch.browser = launch.browser)
  })
}

# create shiny app to put into cluster map
makeShinyApp <- function(port = NULL, shinyApp, launch.browser = FALSE) {
  if(is.null(port)) {
    stop("no port provided")
  }
  result <- NULL
  result$app <- shinyWrapper(shinyApp, launch.browser = launch.browser)
  result$port <- port
  return(result)
}

# if the ports used are not released, use this
# function to kill off all processees listening to
# the port. this is an unwanted caused by clusterMap()
# usually, as we run a shiny app on a specific port
# the port is unlistened when the app is killed
# now we have to do this manually through bash
killAllPortListeners <- function(verbose = SERVER_KILL_VERBOSE) {
  if (verbose) {
    for (i in 1:length(PORTS)) {
      pids <- system(paste(
        "pid=$(lsof -i tcp:",
        PORTS[[i]],
        " -t); echo $pid",
        sep = ""
      ), wait = FALSE, intern = TRUE, ignore.stderr = !SERVER_KILL_VERBOSE, ignore.stdout = !SERVER_KILL_VERBOSE)
      pids <- as.vector(strsplit(pids, " ")[[1]])
      if(length(pids)) {
        print(paste("Killing listener(s) of port ", PORTS[[i]], ": ", paste(pids, collapse = ", "), sep = ""))
      }
    }
  }
  for (i in 1:length(PORTS)) {
    system(paste(
      "pid=$(lsof -i tcp:",
      PORTS[[i]],
      " -t); if [ ! -z $pid ]; then kill -TERM $pid || kill -KILL $pid; fi",
      sep = ""
    ), wait = FALSE, intern = FALSE, ignore.stderr = !SERVER_KILL_VERBOSE, ignore.stdout = !SERVER_KILL_VERBOSE)
  }
}