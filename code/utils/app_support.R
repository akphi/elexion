############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities supporting for apps

############################################################
##                        METHOD                          ##
############################################################

# Plotting the choro
# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
choropeth.percent_map <- function(var, color1, color2, legend.title, min = 0, max = 100) {

  # generate vector of fill colors for map
  shades <- colorRampPalette(c(color1, color2), interpolate = "spline")(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", mar = c(0,0,0,0), xlim=c(-130,-60), ylim=c(20,60))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # # add a legend
  # inc <- (max - min) / 4
  # legend.text <- c(paste0(min, " % or less"),
  #   paste0(min + inc, " %"),
  #   paste0(min + 2 * inc, " %"),
  #   paste0(min + 3 * inc, " %"),
  #   paste0(max, " % or more"))
  
  # legend("bottomleft", 
  #   legend = legend.text, 
  #   fill = shades[c(1, 25, 50, 75, 100)], 
  #   title = legend.title)
}

association.category.by_metric <- function(dataset, threshold.accuracy, threshold.kappa, by, name) {
  return(
    shinyApp(
      ui = fluidPage(
        column(12, align="center",
          visNetworkOutput("network", width = "100%")
        )
      ),
      server = function(input, output) {

        library(shiny)
        library(arules)
        library(arulesViz)
        library(igraph)
        library(visNetwork)

        NUMBER_OF_TOP_CATEGORY_BASKET <- 100

        chosen_groups <- dataset[dataset$accuracy >= threshold.accuracy & dataset$kappa >= threshold.kappa,]
        categories <- c("crime", "demographics", "education", "election", "finance", "healthcare", "occupation", "race", "weather")

        chosen_groups$group <- ""
        for (category in categories) {
          chosen_groups[,category] <- ifelse(chosen_groups[,category], category, "")
          chosen_groups$group <- paste(chosen_groups$group, chosen_groups[,category], sep = " ")
        }

        groups <- strsplit(chosen_groups$group, " ")
        groups <- lapply(groups, function(x){x[!x == ""]})

        trans <- as(groups, "transactions")
        rules = apriori(trans, parameter = list(minlen = 2, support = 0.01, confidence = 0.5, target = "rules"), control = list(verbose = FALSE))
        rules.confSort <- sort(rules, by=by, decreasing=TRUE)

        png(
          mp(RES_MODEL_ANALYZED_PLOT_DIR, name),
          width = MD_LENGTH,
          height = MD_LENGTH
        )
        ig <- plot( rules.confSort[1:10], method = "graph", control = list(type = "items") )
        dev.off()

        # output
        output$network <- renderVisNetwork({
        ig_df <- get.data.frame(ig, what = "both" )
        visNetwork(
          nodes = data.frame(
            id = ig_df$vertices$name
            ,value = ig_df$vertices[, by]
            # ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
            ,ig_df$vertices
          ),
          edges = ig_df$edges,
          width = 1000,
          height = 1000
        ) %>% visOptions( highlightNearest = TRUE, autoResize = FALSE, height = "500px", width = "500px") %>% visEdges( arrows = "to" ) %>% visNodes( shape = "circle", color = "red", title = NULL )
        })
      },
      options = list(launch.browser = FALSE)
    )
  )
}