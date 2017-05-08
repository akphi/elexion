############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities that support interaction with the meta data

############################################################
##                        METHOD                          ##
############################################################

# get name based on var name vector
namev_v <- function(names, meta) {
  return(meta[match(names, meta$var_name),]$name)
}

# get var name based on display name
var_d <- function(name, meta) {
  return(meta[meta$display_name == name,]$var_name)
}

# get var name based on name
var_i <- function(name, meta) {
  return(meta[meta$name == name,]$var_name)
}

# get var name based on display name
var_d <- function(name, meta) {
  return(meta[meta$display_name == name,]$var_name)
}

# display name based on original name
disp <- function(name, meta) {
  return(meta[meta$name == name,]$display_name)
}

# display name based on variable name
disp_v <- function(name, meta) {
  return(meta[meta$var_name == name,]$display_name)
}

# get display names based on original name for a vector
dispv <- function(names, meta) {
  return(meta[match(names, meta$name),]$display_name)
}

# get display names based on variable name for a vector
dispv_v <- function(names, meta) {
  return(meta[match(names, meta$var_name),]$display_name)
}

# change headers of the dataframe from name to display name
dispd <- function(dataset, meta) {
  colnames(dataset) <- dispv(names(dataset), meta)
  return(dataset)
}

# change headers of the dataframe from variable name to display name
dispd_v <- function(dataset, meta) {
  colnames(dataset) <- dispv_v(names(dataset), meta)
  return(dataset)
}

# update or add new instance to the metadata file
metanize <-
  function (metadata,
            name,
            var_name = NULL,
            display_name = NULL,
            category = NULL,
            unit = NULL,
            meaning = NULL,
            formula_latex = NULL,
            lbound = NULL,
            ubound = NULL,
            remark = NULL,
            status = NULL,
            year = NULL,
            source = NULL) {
    if (missing(metadata)) {
      warning("missing metadata input",
              call. = FALSE,
              immediate. = TRUE)
      return()
    }
    if (missing(name) || name == "") {
      warning("metadata name is required",
              call. = FALSE,
              immediate. = TRUE)
      return()
    }
    if (name %in% metadata$name) {
      # edit an entry
      metadata[metadata$name == name,]$var_name <-
        if (is.null(var_name))
          metadata[metadata$name == name,]$var_name
      else
        var_name
      metadata[metadata$name == name,]$display_name <-
        if (is.null(display_name))
          metadata[metadata$name == name,]$display_name
      else
        display_name
      metadata[metadata$name == name,]$category <-
        if (is.null(category))
          metadata[metadata$name == name,]$category
      else
        category
      metadata[metadata$name == name,]$unit <-
        if (is.null(unit))
          metadata[metadata$name == name,]$unit
      else
        unit
      metadata[metadata$name == name,]$meaning <-
        if (is.null(meaning))
          metadata[metadata$name == name,]$meaning
      else
        meaning
      metadata[metadata$name == name,]$formula_latex <-
        if (is.null(formula_latex))
          metadata[metadata$name == name,]$formula_latex
      else
        formula_latex
      metadata[metadata$name == name,]$lbound <-
        if (is.null(lbound))
          metadata[metadata$name == name,]$lbound
      else
        lbound
      metadata[metadata$name == name,]$ubound <-
        if (is.null(ubound))
          metadata[metadata$name == name,]$ubound
      else
        ubound
      metadata[metadata$name == name,]$remark <-
        if (is.null(remark))
          metadata[metadata$name == name,]$remark
      else
        remark
      metadata[metadata$name == name,]$status <-
        if (is.null(status))
          metadata[metadata$name == name,]$status
      else
        status
      metadata[metadata$name == name,]$year <-
        if (is.null(year))
          metadata[metadata$name == name,]$year
      else
        year
      metadata[metadata$name == name,]$source <-
        if (is.null(source))
          metadata[metadata$name == name,]$source
      else
        source
    } else {
      # add new entry
      if (
        is.null(var_name) ||
        is.null(display_name) ||
        is.null(category) ||
        is.null(unit) || 
        is.null(meaning) || 
        is.null(formula_latex) || 
        is.null(lbound) || 
        is.null(ubound) || 
        is.null(remark) || 
        is.null(status) || 
        is.null(year) || 
        is.null(source)
      ) {
        warning(
          "metadata requires all field for new entry",
          call. = FALSE,
          immediate. = TRUE
        )
        return()
      }
      metadata <- as.data.frame(rbind(
        metadata,
        data.frame(
          name = name,
          var_name = var_name,
          display_name = display_name,
          category = category,
          unit = unit,
          meaning = meaning,
          formula_latex = formula_latex,
          lbound = lbound,
          ubound = ubound,
          remark = remark,
          status = status,
          year = year,
          source = source
        )
      ))
    }
    return(metadata)
  }

# metanize for vector
metanize_v <-
  function (metadata,
            name,
            var_name = NULL,
            display_name = NULL,
            category = NULL,
            unit = NULL,
            meaning = NULL,
            formula_latex = NULL,
            lbound = NULL,
            ubound = NULL,
            remark = NULL,
            status = NULL,
            year = NULL,
            source = NULL) {
    for (i in 1:length(name)) {
      metadata <- metanize(
        metadata = metadata,
        name = switch(as.character(length(name)), "0" = NULL, "1" = name, name[i]),
        var_name = switch(as.character(length(var_name)), "0" = NULL, "1" = var_name, var_name[i]),
        display_name = switch(as.character(length(display_name)), "0" = NULL, "1" = display_name, display_name[i]),
        category = switch(as.character(length(category)), "0" = NULL, "1" = category, category[i]),
        unit = switch(as.character(length(unit)), "0" = NULL, "1" = unit, unit[i]),
        meaning = switch(as.character(length(meaning)), "0" = NULL, "1" = meaning, meaning[i]),
        formula_latex = switch(as.character(length(formula_latex)), "0" = NULL, "1" = formula_latex, formula_latex[i]),
        lbound = switch(as.character(length(lbound)), "0" = NULL, "1" = lbound, lbound[i]),
        ubound = switch(as.character(length(ubound)), "0" = NULL, "1" = ubound, ubound[i]),
        remark = switch(as.character(length(remark)), "0" = NULL, "1" = remark, remark[i]),
        status = switch(as.character(length(status)), "0" = NULL, "1" = status, status[i]),
        year = switch(as.character(length(year)), "0" = NULL, "1" = year, year[i]),
        source = switch(as.character(length(source)), "0" = NULL, "1" = source, source[i])
      )
    }
    return(metadata)
  }