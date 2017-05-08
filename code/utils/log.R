############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities for logging and outputing to the console

############################################################
##                        METHOD                          ##
############################################################

# write to log file
log.write <- function(...) {
  print(paste(ifelse(missing(...), "", format(Sys.time(), format =  "%Y-%m-%d:%H.%M.%S")), concat(...), sep = "    "))
  cat(paste(ifelse(missing(...), "", format(Sys.time(), format =  "%Y-%m-%d:%H.%M.%S")), concat(...), "\n", sep = "\t"),
      file = LOG_FILE,
      append = TRUE)
}

# create horizontal rule string made of the desired character
# with length equal to the widht of hte current console window
horizontal_rule <- function(char = "-") {
    suppressWarnings(window_size <- as.numeric(system("tput cols", intern = TRUE)))
    length <- getOption("width")
    if (length(window_size) == 1 && !is.na(window_size[1])) {
        length <- window_size[1]
    }
    return(paste(rep(char, length), collapse = ""))
}