############################################################
##                      DESCRIPTION                       ##
############################################################

# create interactive 3d plot for PCA components

############################################################
##                       APPLICATION                      ##
############################################################

source("configs.R")
source("utils.R")
install_load(c(
    "shiny",
    "rgl"
  )
)

plot.pca <- shinyApp(
  ui = fluidPage(
    column(12, align="center",
      rglwidgetOutput("myWebGL", width = "100%", height = "600px")
    )
  ),
  server = function(input, output) {

    # Library
    library(shiny)
    library(rgl)

    # Data
    dataset <- read.csv(mp(DATA_PREPARED_DIR, "data.csv"), stringsAsFactors = TRUE)
    metadata <- read.csv(mp(DATA_PREPARED_DIR, "meta.csv"), stringsAsFactors = FALSE)
    pca <- read.csv(mp(RES_SELECTED_DIR, "pca_rotation.csv"), stringsAsFactors = FALSE)
    data_pca <- prcomp(dataset[,pca[,1]], center = TRUE, scale. = TRUE)
    target = "elec_rep16_win"

    # IMPORTANT step to generate plot silently
    options(rgl.useNULL = TRUE)

    # Set the window view
    open3d(windowRect = c(0,40,100,100), zoom = 0.6)
    
    # Constants
    axes_scale <- 0.75
    text_scale <- 12.3
    line_scale <- 10
    category_col <- c(
      healthcare = "#0074D9",
      finance = "#FF4136",
      education = "#2ECC40"
    )

    # Points
    points_col <- rep("#111111", length(dataset[, target]))
    points3d(
      data_pca$x[, 1:3],
      size = 0.5,
      col = points_col
    )

    # Features
    features <- data_pca$rotation[, 1:3]
    features_col <- rep("black", nrow(features))
    for (i in 1:nrow(features)) {
      features_col[i] <-
      category_col[metadata[which(metadata$var_name == rownames(features)[i]), ]$category]
    }
    rownames(features) <- dispv_v(rownames(features), metadata)
    text3d(
      features * text_scale,
      texts = rownames(features),
      col = features_col,
      cex = 0.7
    )
    for (i in 1:nrow(features)) {
      arrow3d(
      c(0, 0, 0),
      features[i, ] * line_scale,
      type = "rotation",
      col = features_col[i],
      width = 0.23,
      n = 10,
      barblen = 0.02
      )
    }

    # Axes
    axes_end <- data.frame(
      PC1 = c(axes_scale, 0, 0),
      PC2 = c(0, axes_scale, 0),
      PC3 = c(0, 0, axes_scale)
    )
    rownames(axes_end) <- c("PC1", "PC2", "PC3")
    axes_beg <- data.frame(
      PC1 = c(-axes_scale, 0, 0),
      PC2 = c(0, -axes_scale, 0),
      PC3 = c(0, 0, -axes_scale)
    )
    text3d(
      axes_end * text_scale,
      texts = rownames(axes_end),
      col = "black",
      cex = 0.8
    )
    for (i in 1:nrow(axes_end)) {
      arrow3d(
      axes_beg[i, ] * line_scale,
      axes_end[i, ] * line_scale,
      type = "rotation",
      col = "black",
      width = 0.23,
      n = 10,
      barblen = 0.02
      )
    }

    # Change rotation of the view to make sure we can see all the labels
    par3d(userMatrix = matrix(as.vector(
      c(
      0.5433051,
      0.08459504,
      0.8352624,
      0,
      0.7218699,
      0.4608845,
      -0.516226,
      0,
      -0.4286296,
      0.883419,
      0.1893343,
      0,
      0,
      0,
      0,
      1
      )
    ), ncol = 4))

    # Do not use plot3d() as it will plot. We just want to add stuffs to 
    # the canvas
    scene <- scene3d()
    # Attempt to not allow mouse zoom but failed!
    # scene$par3d$mouseMode <- c("trackball", "none", "none", "none")
    rgl.close()

    # IMPORTANT step to generate plot silently
    save <- options(rgl.inShiny = TRUE)
    on.exit(options(save))

    # output
    output$myWebGL <- renderRglwidget({
      rglwidget(scene)
    })
  },
  options = list(launch.browser = FALSE)
)

runApp(plot.pca, PORTS$PCA_PLOT, quiet = TRUE)