############################################################
##                      DESCRIPTION                       ##
############################################################

# script of miscellaneous tasks

############################################################
##                        LIBRARY                         ##
############################################################

source("configs.R")
source("utils.R")
install_load(c("zoo",
               "formatR"))

############################################################
##                         TASK                           ##
############################################################

# clean all results and generated data files
reset.results <- function() {
  to_remove <- dir(
    path = RES_DIR,
    pattern = "*.*",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(to_remove) != 0) {
    while (TRUE) {
      response <-
        readline("Old results found! Do you want to back them up? [Y/n]")
      if (response == "Y") {
        backup_dir <-
          mp(BACKUP_DIR,
             format(Sys.time(), format =  "%Y-%m-%d_%H.%M.%S"))
        dir.create(backup_dir, showWarnings = TRUE, recursive = TRUE)
        file.copy(
          from = dir(
            path = RES_DIR,
            pattern = "*",
            include.dirs = TRUE,
            full.names = TRUE
          ),
          to = backup_dir,
          copy.date = TRUE,
          recursive = TRUE
        )
        break
      } else if (response == "n") {
        break
      }
    }
    invisible(file.remove(to_remove))
  }
}

# make necessary directories
mkdirs <- function(data = TRUE, result = TRUE) {
  dirs <- c(
    DATA_DIR,
    DATA_MISC_DIR,
    DATA_RAW_DIR,
    DATA_ORIGINAL_DIR,
    DATA_PRE_PROCESSED_DIR,
    DATA_PREPARED_DIR,
    DATA_SELECTED_DIR,
    RES_DIR,
    RES_PRE_PROCESSED_DIR,
    RES_ANALYZED_DIR,
    RES_ANALYZED_PLOT_DIR,
    RES_ANALYZED_HISTORGRAM_DIR,
    RES_PREPARED_DIR,
    RES_FEATURE_RANKED_DIR,
    RES_FEATURE_RANKED_FORMULA_DIR,
    RES_SELECTED_DIR,
    RES_SELECTED_PLOT_DIR,
    RES_MODEL_PROBED_DIR,
    RES_MODEL_PROBED_PLOT_DIR,
    RES_MODEL_TUNED_DIR,
    RES_MODEL_ANALYZED_DIR,
    RES_MODEL_ANALYZED_PLOT_DIR
  )
  sapply(dirs, function(dir) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  })
}

# remove all objects and clean the console
reset.work_space <- function() {
  rm(list = ls(envir = globalenv()), envir = globalenv())
  cat("\014")
  gc()
}

# in testing, still not so unstable, use RStudio formatter instead
format.source_codes <- function() {
  # https://yihui.name/formatr/
  # https://cran.r-project.org/web/packages/formatR/formatR.pdf
  tidy_source(
    source = "misc.R",
    output = TRUE,
    file = "a.R",
    blank = TRUE,
    width.cutoff = 120,
    arrow = FALSE,
    comment = TRUE,
    brace.newline = FALSE,
    indent = 2
  )
}

############################################################
##                         BUILDER                        ##
############################################################

# build all documents
build_documents <- function(remove_dir = FALSE) {
  if (remove_dir) {
    invisible(unlink(dir(
      path = DOCUMENT_DIR,
      pattern = "*",
      recursive = FALSE,
      full.names = TRUE
    ), recursive = TRUE, force = TRUE))
  }
  if ("pdf" %in% DOCUMENT_RENDER_OUTPUT) {
    # need to run all the servers to render the documents with screenshots
    killAllPortListeners()
    # create servers and capture pids
    system2("./start.sh", args = c("-q"), wait = FALSE, stdout = FALSE)
    Sys.sleep(TIME_INTERVAL_DELAY_DOCUMENT_BUILD)
    # build the docs
    setwd(REPORT_DIR)
    source("build.R")
    build(DOCUMENT_RENDER_OUTPUT)
    # kill the pids
    killAllPortListeners()
  } else {
    setwd(REPORT_DIR)
    source("build.R")
    build(DOCUMENT_RENDER_OUTPUT)
  }
  # we need to call this agian since we cleanup at the end of the build script
  source("utils.R")
  setwd(CODE_DIR)
  source("utils.R")
  # Remove temp files - potentially can make this more complicated if needs be
  if (DOCUMENT_RENDER_REMOVE_CACHE) {
    invisible(unlink(dir(
      path = mp(REPORT_DIR),
      pattern = "_bookdown_files",
      recursive = FALSE,
      full.names = TRUE
    ), recursive = TRUE, force = TRUE))
  }
}

# build all documents
build_presentation <- function(remove_dir = FALSE) {
  if (remove_dir) {
    invisible(unlink(dir(
      path = PRESENTATION_DIR,
      pattern = "*",
      recursive = FALSE,
      full.names = TRUE
    ), recursive = TRUE, force = TRUE))
  }
  setwd(SLIDE_DIR)
  source("build.R")
  build()
  setwd(CODE_DIR)
  source("utils.R")
}

############################################################
##                         PARSER                         ##
############################################################

# parse county adjacency data
data_county_adjacency <-
  read.csv(
    mp(DATA_MISC_DIR, "county_adjacency.txt"),
    header = FALSE,
    sep = "\t",
    na.strings = ""
  )
data_county_adjacency <- data_county_adjacency[, c(-1, -3)]
colnames(data_county_adjacency) <- c("code", "neighbor_code")
data_county_adjacency$code <- na.locf(data_county_adjacency$code)
data_county_adjacency <-
  data_county_adjacency[!(data_county_adjacency$code == data_county_adjacency$neighbor_code), ]
write.csv(
  data_county_adjacency,
  file = mp(DATA_MISC_DIR, "county_adjacency.csv"),
  row.names = FALSE
)
rm(data_county_adjacency)

# parse state adjacency data
data_state_fips <-
  read.csv(mp(DATA_ORIGINAL_DIR, "electoral_votes_dist_00.csv"),
           stringsAsFactors = FALSE)
data_state_adjacency <-
  read.csv(mp(DATA_MISC_DIR, "bordering_states.csv"), stringsAsFactors = FALSE)
data_state_adjacency$StateCode <-
  na.locf(data_state_adjacency$StateCode)
data_state_adjacency <-
  merge(
    x = data_state_adjacency,
    y = data_state_fips[, c(1:2)],
    all.X = TRUE,
    by.x = "NeighborStateCode",
    by.y = "code"
  )
colnames(data_state_adjacency)[names(data_state_adjacency) == "fips"] <-
  "neighbor_code"
data_state_adjacency <-
  merge(
    x = data_state_adjacency,
    y = data_state_fips[, c(1:2)],
    all.X = TRUE,
    by.x = "StateCode",
    by.y = "code"
  )
data_state_adjacency <- data_state_adjacency[, c(4, 3)]
colnames(data_state_adjacency)[names(data_state_adjacency) == "fips"] <-
  "code"
data_state_adjacency <-
  data_state_adjacency[!(data_state_adjacency$code == data_state_adjacency$neighbor_code), ]
write.csv(
  data_state_adjacency,
  file = mp(DATA_MISC_DIR, "state_adjacency.csv"),
  row.names = FALSE
)
rm(data_state_fips, data_state_adjacency)

############################################################
##                          CSV                           ##
############################################################

# For some reason, WEKA is not happy with some csv files edited with Excel and so
# we have to import it in R first and export back to CSV to "fix" it
weka.fix_csv <- function(names, path = DATA_ORIGINAL_DIR) {
  for (name in names) {
    data_orig <- read.csv(mp(path, name))
    write.csv(data_orig,
              file = mp(path, name),
              row.names = FALSE)
  }
}

# WEKA use ? for NA so we have to change that
weka.fix_na_csv <- function(names, path = DATA_ORIGINAL_DIR) {
  for (name in names) {
    data_orig <- read.csv(mp(path, name))
    write.csv(
      data_orig,
      file = mp(path, paste(name, ".weka", sep = "")),
      na = "?",
      row.names = FALSE
    )
  }
}