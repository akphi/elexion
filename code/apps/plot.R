############################################################
##                      DESCRIPTION                       ##
############################################################

# apps that allows interaction with graphs and plots

############################################################
##                       APPLICATION                      ##
############################################################

# https://shiny.rstudio.com/articles/plot-interaction.html
ggplot2.zoom.single <- makeShinyApp(
  2306,
  shinyApp(
    ui = fluidPage(
          plotOutput("plot", 
          height = 400,
          width = 600,
            dblclick = "plot_dblclick",
            brush = brushOpts(
              id = "plot_brush",
              resetOnNew = TRUE
            )
        ),
      includeCSS(REPORT_STYLE)
    ),
    server = function(input, output) {
      library(ggplot2)
      library(Cairo)
      # -------------------------------------------------------------------
      # Single zoomable plot (on left)
      ranges <- reactiveValues(x = NULL, y = NULL)

      output$plot <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) +
          geom_point() +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
    },
    options = list(launch.browser = FALSE)
  )
)

ggplo2.zoom.dual <- makeShinyApp(
  2307,
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(width = 8, class = "well",
          h4("Left plot controls right plot"),
          fluidRow(
            column(width = 6,
              plotOutput("plot", height = 300,
                brush = brushOpts(
                  id = "plot_brush",
                  resetOnNew = TRUE
                )
              )
            ),
            column(width = 6,
              plotOutput("plot3", height = 300)
            )
          )
        )
      ),
      includeCSS(REPORT_STYLE)
    ),
    server = function(input, output) {
      library(ggplot2)
      library(Cairo)

      # -------------------------------------------------------------------
      # Linked plots (middle and right)
      ranges2 <- reactiveValues(x = NULL, y = NULL)

      output$plot <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) +
          geom_point()
      })

      output$plot3 <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) +
          geom_point() +
          coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
          ranges2$x <- c(brush$xmin, brush$xmax)
          ranges2$y <- c(brush$ymin, brush$ymax)

        } else {
          ranges2$x <- NULL
          ranges2$y <- NULL
        }
      })

    },
    options = list(launch.browser = FALSE)
  )
)