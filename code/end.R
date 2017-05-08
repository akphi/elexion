############################################################
##                      DESCRIPTION                       ##
############################################################

# script to end the app server (violently) by killing all
# the ports used

############################################################
##                        LIBRARY                         ##
############################################################

source("misc.R")

############################################################
##                         TASK                           ##
############################################################

# whether to show violent kill messages or not
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0) {
    args[1] <- SERVER_KILL_VERBOSE
}

killAllPortListeners(verbose = as.logical(args[1]))