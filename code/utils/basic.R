############################################################
##                      DESCRIPTION                       ##
############################################################

# basic utilities for the script, does not depend on any
# third-party packages (so it can store the install_load
# function in it)

############################################################
##                        METHOD                          ##
############################################################

# check if installation is need and load the packages
# https://gist.github.com/stevenworthington/3178163
# packages from github requires different method
# require() vs. library() 
# http://stackoverflow.com/questions/5595512/what-is-the-difference-between-require-and-library
install_load <- function(pkg, verbose = LOADER_VERBOSE, dependencies = TRUE) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    if (!LOADER_CRAN_GRAPHIC_ENABLED) {
      chooseCRANmirror(graphic = FALSE, ind = LOADER_CRAN_MIRROR);
    }
    if (!dependencies) {
      install.packages(new.pkg) 
    } else {
      install.packages(new.pkg, dependencies = TRUE)
    }
  }
  if (LOADER_VERBOSE) {
    sapply(pkg, require, character.only = TRUE)
  } else {
    invisible(sapply(pkg, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}

install_load_source  <- function(pkg, verbose = LOADER_VERBOSE, dependencies = TRUE) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    if (!LOADER_CRAN_GRAPHIC_ENABLED) {
      chooseCRANmirror(graphic = FALSE, ind = LOADER_CRAN_MIRROR);
    }
    if (!dependencies) {
      install.packages(new.pkg, type = "source") 
    } else {
      install.packages(new.pkg, dependencies = TRUE, type = "source")
    }
  }
  if (LOADER_VERBOSE) {
    sapply(pkg, require, character.only = TRUE)
  } else {
    invisible(sapply(pkg, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}

install_load_github <- function(pkg, path, verbose = LOADER_VERBOSE, force = FALSE) {
  library("devtools")
  if (sum(!(pkg %in% installed.packages()[, "Package"])) > 0) {
    devtools::install_github(path[!(pkg %in% installed.packages()[, "Package"])], force = force)
  }
  if (LOADER_VERBOSE) {
    sapply(pkg, require, character.only = TRUE)
  } else {
    invisible(sapply(pkg, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
}

# notin function
# http://stackoverflow.com/questions/5831794/opposite-of-in
'%notin%' <- function(x, y)
  ! ('%in%'(x, y))

# concat string without space between phrases
concat <- function(...) {
  return(paste(..., sep = ""))
}

# make path
mp <- function(...) {
  return(paste(list(...), collapse = "/"))
}

# nan to zero
nan_to_zero <- function(input) {
  return(sapply(input, FUN = function(x) ifelse(is.nan(x), 0, x)))
}

# round number in the input vector to the closest number in the target vector
vround <- function(input, target) {
  result <- sapply(input, function(x) {
    return(target[which.min(abs(target - x))])
  })
  return(result)
}

# get rid of stuffs inside a vector
getrov <- function(input, to_drop) {
  return(input[input %notin% to_drop])
}

# get rid of stuffs inside a dataframe
getrod <- function(input, to_drop) {
  return(input[, names(input) %notin% to_drop])
}

# trim vector of strings down to a limited length
trim_strings <- function(strings, length, elip = FALSE) {
  results <- substr(strings, start = 1, stop = length)
  if (elip) {
    results[nchar(strings) > length] = paste(results[nchar(strings) > length], "...", sep = "")
  }
  return(results)
}

# combine list of dataframes into a single dataframe
combine_dataframe_list <- function(results) {
  combined <- NULL
  for (i in 1:length(results)) {
    combined <- rbind(combined, results[[i]])
  }
  return(combined)
}