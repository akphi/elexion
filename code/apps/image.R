############################################################
##                      DESCRIPTION                       ##
############################################################

# apps that create servers to serve documents, presentations
# etc.

############################################################
##                       APPLICATION                      ##
############################################################

# click on the zoom button to make the image zoomable
# example of zoom_settings are
# 'zoomType: inner, cursor: "pointer"'
# 'zoomType: "lens", cursor: "pointer"'
# 'zoomWindowPosition: 3, zoomWindowHeight: 200, zoomWindowWidth: 200, borderSize: 2, cursor: "pointer"'
# more can be found at http://www.elevateweb.co.uk/image-zoom/examples
image.zoom <- function(port, file = "", width = 500, height = 500, zoom_settings) {
  return(
    makeShinyApp(
      port,
      shinyApp(
        ui = fluidPage(
          # tags$head(
          #   tags$script(src = "http://www.elevateweb.co.uk/wp-content/themes/radial/jquery.elevatezoom.min.js")
          # ),
          # actionButton("myBtn", "<i>ma</i>"),
          imageOutput("myImage", hover = "image_hover"),
          singleton(tags$head(
            tags$script(
              paste(
              'Shiny.addCustomMessageHandler("testmessage",
              function(message) {
                $("#myImage img").elevateZoom({', zoom_settings,'});
              }
            );', sep = "")
            ))),
          includeScript("./apps/assets/jquery.elevatezoom.min.js"),
          includeCSS(REPORT_STYLE)
        ),
        server = function(input, output, session) {
          output$myImage <- renderImage({
            list(src = file,
              width = width,
              height = height,
              alt = "Image not found")
      }, deleteFile = FALSE)
          # observe({
          #   if (input$image_dblclick) {
          #     session$sendCustomMessage(type = 'testmessage', message = list())
          #   }
          # })
          observeEvent(input$image_hover, {
            session$sendCustomMessage(type = 'testmessage', message = list())
          })
        },
        options = list(launch.browser = FALSE)
      )
    )
  )
}