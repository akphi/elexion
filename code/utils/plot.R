############################################################
##                      DESCRIPTION                       ##
############################################################

# utilities that helps with data visualization

############################################################
##                        METHOD                          ##
############################################################

# plot correlation plot for every pair in the given list
plot.pairwise_correlation_scatter <-
  function(dataset, meta, list, path, size = MD_LENGTH) {
    for (i in 0:(length(list) / 2 - 1)) {
      png(mp(path, paste(
        var_i(list[2 * i + 1], meta),
        "-",
        var_i(list[2 * i + 2], meta),
        ".png",
        sep = ""
      )), width = size, height = size)
      plot(
        x = as.numeric(dataset[, list[2 * i + 1]]),
        y = as.numeric(dataset[, list[2 * i + 2]]),
        main = paste(
          disp(list[2 * i + 1], meta),
          "-",
          disp(list[2 * i + 2], meta),
          "( Pearson's r =",
          round(
            cor(as.numeric(dataset[, list[2 * i + 1]]),
                as.numeric(dataset[, list[2 * i + 2]]),
                use = "pairwise.complete.obs"),
            digits = 2
          ),
          ")",
          sep = " "
        ),
        xlab = disp(list[2 * i + 1], meta),
        ylab = disp(list[2 * i + 2], meta)
      )
      dev.off()
    }
  }

# plot histograms of all features of the given dataset
plot.histogram <- function(dataset, meta, path) {
  for (colname in colnames(dataset)) {
    png(mp(path, paste(var_i(colname, meta), ".png", sep = "")), width = 900, height = 900)
    hist(dataset[, colname],
         main = disp(colname, meta),
         breaks = 100)
    dev.off()
  }
}

# modify ggbiplot because it is not great
# http://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
ggbiplot2 <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
         obs.scale = 1 - scale, var.scale = scale, 
         groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
         labels = NULL, labels.size = 3, alpha = 1, 
         point.size = 1,
         point.alpha = 1, 
         var.axes = TRUE, 
         circle = FALSE, 
         circle.size = 0.5,
         circle.prob = 0.69, 
         circle.color = muted("red"),
         circle.alpha = 1,
         varname = NULL, 
         varname.size = 3, varname.adjust = 1.5, 
         varname.abbrev = FALSE, 
         varname.color = muted("red"),
         arrow.color = muted("red"),
         arrow.line = "solid",
         arrow.size = 1,
         arrow.alpha = 1,
         ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)

  stopifnot(length(choices) == 2)

  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else {
    stop('Expected a object of class prcomp, princomp or PCA')
  }

  # Scores
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])

  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)

  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }

  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  # r <- 1
  r <- circle.prob

  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- df.v / sqrt(max(v.scale))

  ## Scale Scores
  r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
  df.u=.99*df.u/r.scale

  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }

  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))

  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }

  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }

  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }

  if(!is.null(varname)) {
    df.v$varname <- varname
  }

  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust[1] * sign(xvar)) / 2)
  df.v$vjust = with(df.v, (1 - varname.adjust[2] * sign(xvar)) / 2)

  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  
  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha, size = point.size)
    } else {
      g <- g + geom_point(alpha = alpha, size = point.size)      
    }
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))

    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) < 2) {
        return(NULL)
      } else if(nrow(x) == 2) {
        sigma <- var(cbind(x$xvar, x$yvar))
      } else {
        sigma <- diag(c(var(x$xvar), var(x$yvar)))
      }
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }

  # Label the variable axes
  if(var.axes) {
    g <- g + 
      geom_text(data = df.v, 
                aes(label = varname, x = xvar, y = yvar, 
                    angle = angle, hjust = hjust, vjust = vjust), 
                color = varname.color, size = varname.size)
  }

  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }

  # TODO: Add a second set of axes

  # Draw the axes
  if(var.axes) {
    # Draw circle
    if(!is.null(circle)) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = circle.color, 
                         size = circle.size, alpha = circle.alpha)
    }

    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(arrow.size, 'picas')), 
                   color = arrow.color, linetype = arrow.line, alpha = arrow.alpha)
  }

  return(g)
}

# problem with saving plots in neuralnet
# http://stackoverflow.com/questions/11866740/having-problems-saving-a-neural-net-plot-using-neuralnet-package-r
# After speaking with Frauke Guenther (Maintainer of neuralnet), she explains the problem lies with neuralnet's 
# ability to plot more than one neural net at the same time.
# The plot(..., file = "") should have saved the file to disk, however does not seem to function properly, a fix 
# will be patched soon to the package itself, however for the mean time she kindly provided a quick plot.nn() fix 
# that would allow one to save the plot to disk.
plot.nn <-
function (x, rep = NULL, x.entry = NULL, x.out = NULL, radius = 0.15, 
    arrow.length = 0.2, intercept = TRUE, intercept.factor = 0.4, 
    information = TRUE, information.pos = 0.1, col.entry.synapse = "black", 
    col.entry = "black", col.hidden = "black", col.hidden.synapse = "black", 
    col.out = "black", col.out.synapse = "black", col.intercept = "blue", 
    fontsize = 12, dimension = 6, show.weights = TRUE, file = NULL, 
    ...) 
{
    net <- x
    if (is.null(net$weights)) 
        stop("weights were not calculated")
    if (!is.null(file) && !is.character(file)) 
        stop("'file' must be a string")
    if (is.null(rep)) {
        for (i in 1:length(net$weights)) {
            if (!is.null(file)) 
                file.rep <- paste(file, ".", i, sep = "")
            else file.rep <- NULL
            #dev.new()
            plot.nn(net, rep = i, x.entry, x.out, radius, arrow.length, 
                intercept, intercept.factor, information, information.pos, 
                col.entry.synapse, col.entry, col.hidden, col.hidden.synapse, 
                col.out, col.out.synapse, col.intercept, fontsize, 
                dimension, show.weights, file.rep, ...)
        }
    }
    else {
        if (is.character(file) && file.exists(file)) 
            stop(sprintf("%s already exists", sQuote(file)))
        result.matrix <- t(net$result.matrix)
        if (rep == "best") 
            rep <- as.integer(which.min(result.matrix[, "error"]))
        if (rep > length(net$weights)) 
            stop("'rep' does not exist")
        weights <- net$weights[[rep]]
        if (is.null(x.entry)) 
            x.entry <- 0.5 - (arrow.length/2) * length(weights)
        if (is.null(x.out)) 
            x.out <- 0.5 + (arrow.length/2) * length(weights)
        width <- max(x.out - x.entry + 0.2, 0.8) * 8
        radius <- radius/dimension
        entry.label <- net$model.list$variables
        out.label <- net$model.list$response
        neuron.count <- array(0, length(weights) + 1)
        neuron.count[1] <- nrow(weights[[1]]) - 1
        neuron.count[2] <- ncol(weights[[1]])
        x.position <- array(0, length(weights) + 1)
        x.position[1] <- x.entry
        x.position[length(weights) + 1] <- x.out
        if (length(weights) > 1) 
            for (i in 2:length(weights)) {
                neuron.count[i + 1] <- ncol(weights[[i]])
                x.position[i] <- x.entry + (i - 1) * (x.out - 
                  x.entry)/length(weights)
            }
        y.step <- 1/(neuron.count + 1)
        y.position <- array(0, length(weights) + 1)
        y.intercept <- 1 - 2 * radius
        information.pos <- min(min(y.step) - 0.1, 0.2)
        if (length(entry.label) != neuron.count[1]) {
            if (length(entry.label) < neuron.count[1]) {
                tmp <- NULL
                for (i in 1:(neuron.count[1] - length(entry.label))) {
                  tmp <- c(tmp, "no name")
                }
                entry.label <- c(entry.label, tmp)
            }
        }
        if (length(out.label) != neuron.count[length(neuron.count)]) {
            if (length(out.label) < neuron.count[length(neuron.count)]) {
                tmp <- NULL
                for (i in 1:(neuron.count[length(neuron.count)] - 
                  length(out.label))) {
                  tmp <- c(tmp, "no name")
                }
                out.label <- c(out.label, tmp)
            }
        }
        grid.newpage()
        pushViewport(viewport(name = "plot.area", width = unit(dimension, 
            "inches"), height = unit(dimension, "inches")))
        for (k in 1:length(weights)) {
            for (i in 1:neuron.count[k]) {
                y.position[k] <- y.position[k] + y.step[k]
                y.tmp <- 0
                for (j in 1:neuron.count[k + 1]) {
                  y.tmp <- y.tmp + y.step[k + 1]
                  result <- calculate.delta(c(x.position[k], 
                    x.position[k + 1]), c(y.position[k], y.tmp), 
                    radius)
                  x <- c(x.position[k], x.position[k + 1] - result[1])
                  y <- c(y.position[k], y.tmp + result[2])
                  grid.lines(x = x, y = y, arrow = arrow(length = unit(0.15, 
                    "cm"), type = "closed"), gp = gpar(fill = col.hidden.synapse, 
                    col = col.hidden.synapse, ...))
                  if (show.weights) 
                    draw.text(label = weights[[k]][neuron.count[k] - 
                      i + 2, neuron.count[k + 1] - j + 1], x = c(x.position[k], 
                      x.position[k + 1]), y = c(y.position[k], 
                      y.tmp), xy.null = 1.25 * result, color = col.hidden.synapse, 
                      fontsize = fontsize - 2, ...)
                }
                if (k == 1) {
                  grid.lines(x = c((x.position[1] - arrow.length), 
                    x.position[1] - radius), y = y.position[k], 
                    arrow = arrow(length = unit(0.15, "cm"), 
                      type = "closed"), gp = gpar(fill = col.entry.synapse, 
                      col = col.entry.synapse, ...))
                  draw.text(label = entry.label[(neuron.count[1] + 
                    1) - i], x = c((x.position - arrow.length), 
                    x.position[1] - radius), y = c(y.position[k], 
                    y.position[k]), xy.null = c(0, 0), color = col.entry.synapse, 
                    fontsize = fontsize, ...)
                  grid.circle(x = x.position[k], y = y.position[k], 
                    r = radius, gp = gpar(fill = "white", col = col.entry, 
                      ...))
                }
                else {
                  grid.circle(x = x.position[k], y = y.position[k], 
                    r = radius, gp = gpar(fill = "white", col = col.hidden, 
                      ...))
                }
            }
        }
        out <- length(neuron.count)
        for (i in 1:neuron.count[out]) {
            y.position[out] <- y.position[out] + y.step[out]
            grid.lines(x = c(x.position[out] + radius, x.position[out] + 
                arrow.length), y = y.position[out], arrow = arrow(length = unit(0.15, 
                "cm"), type = "closed"), gp = gpar(fill = col.out.synapse, 
                col = col.out.synapse, ...))
            draw.text(label = out.label[(neuron.count[out] + 
                1) - i], x = c((x.position[out] + radius), x.position[out] + 
                arrow.length), y = c(y.position[out], y.position[out]), 
                xy.null = c(0, 0), color = col.out.synapse, fontsize = fontsize, 
                ...)
            grid.circle(x = x.position[out], y = y.position[out], 
                r = radius, gp = gpar(fill = "white", col = col.out, 
                  ...))
        }
        if (intercept) {
            for (k in 1:length(weights)) {
                y.tmp <- 0
                x.intercept <- (x.position[k + 1] - x.position[k]) * 
                  intercept.factor + x.position[k]
                for (i in 1:neuron.count[k + 1]) {
                  y.tmp <- y.tmp + y.step[k + 1]
                  result <- calculate.delta(c(x.intercept, x.position[k + 
                    1]), c(y.intercept, y.tmp), radius)
                  x <- c(x.intercept, x.position[k + 1] - result[1])
                  y <- c(y.intercept, y.tmp + result[2])
                  grid.lines(x = x, y = y, arrow = arrow(length = unit(0.15, 
                    "cm"), type = "closed"), gp = gpar(fill = col.intercept, 
                    col = col.intercept, ...))
                  xy.null <- cbind(x.position[k + 1] - x.intercept - 
                    2 * result[1], -(y.tmp - y.intercept + 2 * 
                    result[2]))
                  if (show.weights) 
                    draw.text(label = weights[[k]][1, neuron.count[k + 
                      1] - i + 1], x = c(x.intercept, x.position[k + 
                      1]), y = c(y.intercept, y.tmp), xy.null = xy.null, 
                      color = col.intercept, alignment = c("right", 
                        "bottom"), fontsize = fontsize - 2, ...)
                }
                grid.circle(x = x.intercept, y = y.intercept, 
                  r = radius, gp = gpar(fill = "white", col = col.intercept, 
                    ...))
                grid.text(1, x = x.intercept, y = y.intercept, 
                  gp = gpar(col = col.intercept, ...))
            }
        }
        if (information) 
            grid.text(paste("Error: ", round(result.matrix[rep, 
                "error"], 6), "   Steps: ", result.matrix[rep, 
                "steps"], sep = ""), x = 0.5, y = information.pos, 
                just = "bottom", gp = gpar(fontsize = fontsize + 
                  2, ...))
        popViewport()
        if (!is.null(file)) {
            weight.plot <- recordPlot()
            save(weight.plot, file = file)
        }
    }
}
calculate.delta <-
function (x, y, r) 
{
    delta.x <- x[2] - x[1]
    delta.y <- y[2] - y[1]
    x.null <- r/sqrt(delta.x^2 + delta.y^2) * delta.x
    if (y[1] < y[2]) 
        y.null <- -sqrt(r^2 - x.null^2)
    else if (y[1] > y[2]) 
        y.null <- sqrt(r^2 - x.null^2)
    else y.null <- 0
    c(x.null, y.null)
}
draw.text <-
function (label, x, y, xy.null = c(0, 0), color, alignment = c("left", 
    "bottom"), ...) 
{
    x.label <- x[1] + xy.null[1]
    y.label <- y[1] - xy.null[2]
    x.delta <- x[2] - x[1]
    y.delta <- y[2] - y[1]
    angle = atan(y.delta/x.delta) * (180/pi)
    if (angle < 0) 
        angle <- angle + 0
    else if (angle > 0) 
        angle <- angle - 0
    if (is.numeric(label)) 
        label <- round(label, 5)
    pushViewport(viewport(x = x.label, y = y.label, width = 0, 
        height = , angle = angle, name = "vp1", just = alignment))
    grid.text(label, x = 0, y = unit(0.75, "mm"), just = alignment, 
        gp = gpar(col = color, ...))
    popViewport()
}