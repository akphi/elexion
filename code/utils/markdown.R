############################################################
##                      DESCRIPTION                       ##
############################################################

# text utilities to be used with RMarkdown

############################################################
##                        METHOD                          ##
############################################################

# convert a vector to md link that is labeled
md_linkify <- function(input, text = "view") {
  return(paste("[", text, "](", input, ")", sep = ""))
}

# convert a vector to latex link that is labeled
tex_linkify <- function(input, text = "view") {
  return(paste("\\href{", input, "}{", text, "}", sep = ""))
}

# how to deal with a pdf document with table in Rmarkdown
# ``` {r table-main_source, include = TRUE, echo = FALSE, results = "asis"}
# temp <- main_source
# if (RENDER_MODE=="pdf") {
#   temp[,"URL"] <- main_source[,"URL"]
#   XT <- xtable::xtable(
#     temp,
#     # caption = "Hwarang",
#     align=c(
#       "p{0.015\\textwidth}",
#       "p{0.4\\textwidth}",
#       "p{0.585\\textwidth}"
#     ),
#     auto = TRUE,
#     comment = FALSE
#   )
#   print.xtable(
#     XT,
#     type = "latex",
#     tabular.environment="longtable",
#     floating=FALSE,
#     html.table.attributes = getOption("xtable.html.table.attributes", "border = 0"),
#     comment=F, include.rownames=F, 
#     size=getOption("xtable.size", "normalsize")
#   )
# } else {
#   temp[,"URL"] <- md_linkify(main_source[,"URL"])
#   knitr::kable(
#     temp,
#     col.names = c("Dataset", "Source"),
#     align=c("l", "c"),
#     digits = 2,
#     booktabs = TRUE,
#     format = "html"
#     )
# }
# ```

# resize table column (relatively) using kable
# https://github.com/rstudio/bookdown/issues/122
# example usage knitr::kable(...) %>% html_table_width(c(...))
html_table_width <- function(kable_output, width){
  width_html <- paste0(paste0('<col width="', width, '">'), collapse = "\n")
  sub("<table>", paste0("<table>\n", width_html), kable_output)
}