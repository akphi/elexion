############################################################
##                      DESCRIPTION                       ##
############################################################

# apps that shows the choropeth of each features from the
# dataset

############################################################
##                       APPLICATION                      ##
############################################################

# data pre processed
choropeth.by_county <- function(port, dataset, metadata, exclude, color = names(COLOR)) {
  library(maps)
  counties_name <- maps::county.fips
  data <- merge(counties_name, dataset, all.x = TRUE, by = "fips")
  data <- data[, names(data) %notin% append("polyname", exclude)]
  attributes <- metadata[metadata$name %in% names(data), ]$display_name
  return(
    makeShinyApp(
      port, 
      shinyApp(
        ui = pageWithSidebar(
          headerPanel(NULL),
          sidebarPanel(fluidRow(
            column(
              8,
              selectInput(
                "var",
                label = "Feature to display",
                choices = attributes,
                selected = "Percent White",
                width = "100%"
              )
            ),
            column(
              2,
              selectInput(
                "col1",
                label = "Min. color",
                choices = color,
                selected = color[1]
              )
            ),
            column(
              2,
              selectInput(
                "col2",
                label = "Max. color",
                choices = color,
                selected = color[length(color)]
              )
            )
          ), width = 12),
          mainPanel(plotOutput("map", height = 600), width = 12)
        ),
        server = function(input, output) {
          library(maps)
          output$map <- renderPlot({
            data_input <- data[, metadata[metadata$display_name == input$var,]$name]
            choropeth.percent_map(var = data_input, color1 = COLOR[input$col1], color2 = COLOR[input$col2], legend.title = "", max = max(data_input, na.rm = TRUE), min = min(data_input, na.rm = TRUE))
          })
        },
        options = list(launch.browser = FALSE)
      )
    )
  )
}

# data pre processed
choropeth.by_county.with_preset <- function(port, dataset, metadata, exclude, color = names(COLOR)) {
  library(maps)
  counties_name <- maps::county.fips
  data <- merge(counties_name, dataset, all.x = TRUE, by = "fips")
  data <- data[, names(data) %notin% append("polyname", exclude)]
  # attributes <- metadata[metadata$name %in% names(data), ]$display_name
  attributes <- c(
    "Republican Win 2016",
    "Republican Win 2012",
    "Republican Win 2008",
    "Population",
    "Democratic Votes 2016",
    "Republican Votes 2016",
    "White not Latino Population",
    "African American Population",
    "African American Population",
    "Latino Population",
    "Adult Obesity Rate",
    "Homogeneity Index",
    "Uninsured Rate",
    "Unemployment Rate",
    "Winter Avg. Precipitation",
    "Winter Avg. Temperature"
  )
  return(
    makeShinyApp(
      port, 
      shinyApp(
        ui = pageWithSidebar(
          headerPanel(NULL),
          sidebarPanel(fluidRow(
            column(
              12,
              # selectInput(
              #   "var",
              #   label = "",
              #   choices = attributes,
              #   selected = "Percent White",
              #   width = "100%"
              # )
              radioButtons("var", "Presets:",
               attributes)
            )), width = 3),
          mainPanel(plotOutput("map", height = 600), width = 9)
        ),
        server = function(input, output) {
          library(maps)
          output$map <- renderPlot({
            data_input <- data[, metadata[metadata$display_name == input$var,]$name]
            col1 <- switch(input$var,
              "Republican Win 2016" = "Dem. Blue",
              "Republican Win 2012" = "Dem. Blue",
              "Republican Win 2008" = "Dem. Blue",
              "Population" = "Green",
              "Democratic Votes 2016" = "Aqua",
              "Republican Votes 2016" = "Orange",
              "White not Latino Population" = "Aqua",
              "African American Population" = "Teal",
              "African American Population" = "Orange",
              "Latino Population" = "Yellow",
              "Adult Obesity Rate" = "Teal",
              "Homogeneity Index" = "Orange",
              "Uninsured Rate" = "Lime",
              "Unemployment Rate" = "Orange",
              "Winter Avg. Precipitation" = "Aqua",
              "Winter Avg. Temperature"  = "Yellow"
            )
            col2 <- switch(input$var,
              "Republican Win 2016" = "Rep. Red",
              "Republican Win 2012" = "Rep. Red",
              "Republican Win 2008" = "Rep. Red",
              "Population" = "Black",
              "Democratic Votes 2016" = "Navy",
              "Republican Votes 2016" = "Black",
              "White not Latino Population" = "Maroon",
              "African American Population" = "Maroon",
              "African American Population" = "Navy",
              "Latino Population" = "Maroon",
              "Adult Obesity Rate" = "Rep. Red",
              "Homogeneity Index" = "Navy",
              "Uninsured Rate" = "Maroon",
              "Unemployment Rate" = "Navy",
              "Winter Avg. Precipitation" = "Fuchsia",
              "Winter Avg. Temperature"  = "Red"
            )
            choropeth.percent_map(var = data_input, color1 = COLOR[col1], color2 = COLOR[col2], legend.title = "", max = max(data_input, na.rm = TRUE), min = min(data_input, na.rm = TRUE))
          })
        },
        options = list(launch.browser = FALSE)
      )
    )
  )
}