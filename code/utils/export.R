############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities for exporting R objects 

############################################################
##                        METHOD                          ##
############################################################

# write multiple csv files to the same path (can be exploited
# for more path of course)
write.csvs <- function(data_list, file_names, path, ...) {
  if ((length(data_list) == length(file_names))) {
    for (i in 1:length(data_list)) {
      write.csv(data_list[i],
                file = mp(path, paste(file_names[i], ".csv", sep = "")),
                ...)
    }
  }
}


# export formula for evaluator outcome
export.evaluator.result <-
  function(data_list,
           file_name,
           file_path,
           formula_names,
           formula_path) {
    # export formula
    formula <- lapply(data_list, function(x) {
      return(x$formula)
    })
    write.csvs(formula,
               paste(file_name, formula_names, sep = "."),
               formula_path,
               row.names = FALSE)
    # export counts
    result <- data_list[[1]]$counts
    colnames(result) <- c("name", formula_names[[1]])
    for (i in 2:length(data_list)) {
      colnames(data_list[[i]]$counts) <- c("name", formula_names[i])
      result <-
        merge(
          x = result,
          y = data_list[[i]]$counts,
          by = "name",
          all = TRUE
        )
    }
    write.csv(result,
              file = mp(file_path, paste(file_name, ".csv", sep = "")),
              row.names = FALSE)
  }
