#' @name gmm_basket_shot_chart
#' @aliases gmm_basket_shot_chart
#' 
#' @title Gaussian mixture model for model-based shot density chart
#' 
#' @description 
#' TODO
#'
#' @param data TODO
#' @param G TODO
#' @param modelNames TODO
#' @param lambda TODO
#' @param ... TODO
#'
#' @return TODO
#'
#' @examples
#' # TODO
#' 
#' @export

gmm_basket_shot_chart <- function(data, 
                                  G = 1:9, 
                                  modelNames = "VVV",
                                  prior = priorControl("defaultPrior"),
                                  lambda = c(0,0), 
																	nstart = 100,
                                  ...)
{

  data <- data.matrix(data)
  stopifnot("x" %in% colnames(data) & "y" %in% colnames(data))
  data <- data[,c("x", "y")]
  # constrain data coordinates to midfield size
  bounds <- list(x = c(-25, 25), y = c(0,47))
  eps <- sqrt(.Machine$double.eps)
  data <- na.omit(data)
  data[,"x"] <- pmin(pmax(data[,"x"], bounds$x[1]+eps), bounds$x[2]-eps)
  data[,"y"] <- pmin(pmax(data[,"y"], bounds$y[1]+eps), bounds$y[2]-eps)
  
  object <- densityMclustBounded(data, G = G, modelNames = modelNames,
                                 lbound = sapply(bounds, min),
                                 ubound = sapply(bounds, max),
                                 lambda = lambda, 
																 nstart = nstart, ...)
  class(object) <- c("GMMBasketShotChart", class(object))
  return(object)
}

#' @name gmm_basket_shot_chart_predict
#' @aliases gmm_basket_shot_chart_predict
#' 
#' @title Predictions for mixture-based shot density charts
#' 
#' @description 
#' TODO
#'
#' @param x TODO
#' @param y TODO
#' @param newdata TODO
#' @param ... TODO
#'
#' @return TODO
#'
#' @examples
#' # TODO
#' 
#' @export

gmm_basket_shot_chart_predict <- function(x, y, newdata, ...)
{
  if(inherits(x, "GMMBasketShotChart"))
  {  
    dens1 <- x
    x <- dens1$data
  } else
  {
    x <- as.matrix(x)
    dens1 <- gmm_basket_shot_chart(x, ...)
  }

  if(inherits(y, "GMMBasketShotChart"))
  {  
    dens0 <- y
    y <- dens0$data
  } else
  {
    y <- as.matrix(y)
    dens0 <- gmm_basket_shot_chart(y, ...)
  }

  newdata <- if(missing(newdata)) rbind(x, y) else as.matrix(newdata)
  stopifnot("x" %in% colnames(newdata) & "y" %in% colnames(newdata))
  newdata <- newdata[,c("x", "y")]

  pro       <- c(nrow(x), nrow(y))/nrow(newdata)
  logcdens  <- cbind(predict(dens1, newdata = newdata, 
                             what = "dens", logarithm = TRUE),
                     predict(dens0, newdata = newdata, 
                             what = "dens", logarithm = TRUE))
  Probs     <- softmax(logcdens, log(pro))[,1]
  ShotType  <- ifelse(apply(newdata, 1, three_point_shot), "3P", "2P")
  ExpPoints <- Probs*ifelse(ShotType == "3P", 3, 2)
  out <- list(data = newdata, dens1 = dens1, dens0 = dens0,
              Prop = pro, Probs = Probs, 
              ShotType = ShotType, ExpPoints = ExpPoints)
  return(out)
}


#' @name plot.GMMBasketShotChart
#' @aliases plot.GMMBasketShotChart
#' 
#' @title ggplot method for ... TODO
#' 
#' @description 
#' Plots based on ggplot2 for ... TODO
#' 
#' @param object  an object of class `GMMBasketShotChart`
#' @param prob TODO
#' @param ngrid TODO
#' @param theme TODO
#' @param palette TODO
#' @param addpoints TODO
#' @param ...  additional arguments to be passed to the low level functions.
#' 
#' @return A ggplot object.
#' 
#' @seealso [gmm_basket_shot_chart()]
#'
#' @examples
#' TODO
#' 
#' @export

plot.GMMBasketShotChart <- function(x, 
                                    prob = c(0.25, 0.5, 0.75, 0.9), 
                                    ngrid = 100, 
                                    theme = c("light", "dark"),
                                    palette = "YlGnBu",
                                    addpoints = FALSE,
                                    ...)
{
  object <- x # Argh. Really want to use object anyway
  stopifnot(inherits(object, "GMMBasketShotChart"))

  prob <- sort(unique(c(0, prob[prob >= 0 & prob <= 1])))
  theme <- match.arg(theme, eval(formals(plot.GMMBasketShotChart)$theme),
                     several.ok = FALSE)
  ngrid <- as.integer(ngrid)[1]
  xygrid <- expand.grid(seq(object$lbound[1], object$ubound[1], length = ngrid),
                        seq(object$lbound[2], object$ubound[2], length = ngrid))
  pred <- matrix(predict(object, newdata = xygrid), ngrid, ngrid)
  brks <- mclust::hdrlevels(object$density, prob)
  brks[1] <- max(brks[1], pred)
  # brks[length(brks)] <- min(brks[length(brks)], pred)
  hrd_labels <- paste0(format(prob[-1]*100, trim = TRUE, digits = 2), "%")
  
  plot <- ggplot() +
    stat_contour_filled(data = data.frame(x = xygrid[,1],
                                          y = xygrid[,2],
                                          z = as.vector(pred)),
                        aes(x = x, y = y, z = z),
                        breaks = brks) +
    scale_fill_brewer(name = "\nHDRs:  ",
                      labels = hrd_labels,
                      palette = palette,
                      direction = -1) +
    plot_basket_court(theme, add = TRUE) +
    theme(legend.position = "right")

  if(addpoints)
  {
    plot <- plot + 
      geom_point(data = data.frame(x = object$data[,1],
                                   y = object$data[,2]),
                 aes(x, y),
                 alpha = 1, pch = 16, size = 1,
                 col = if(theme == "dark") "white" else "gray20")
  }
  
  return(plot)
}


#' @name gmm_basket_shot_chart_calibration
#' @aliases gmm_basket_shot_chart_calibration
#' 
#' @title TODO ...
#' 
#' @description 
#' TODO ...
#' 
#' @param x TODO 
#' @param y TODO
#' @param ...  additional arguments to be passed to the low level functions.
#' 
#' @return A list of elements:
#' 
#' @seealso [gmm_basket_shot_chart(), gmm_basket_shot_chart_predict()]
#'
#' @examples
#' TODO
#' 
#' @export

gmm_basket_shot_chart_calibration <- function(x, y, 
                                              palette = basket_offensive_areas_theme,
                                              ...)
{
  if(inherits(x, "GMMBasketShotChart"))
  {  
    dens1 <- x
    x <- dens1$data
  } else
  {
    x <- as.matrix(x)
    dens1 <- gmm_basket_shot_chart(x, ...)
  }

  if(inherits(y, "GMMBasketShotChart"))
  {  
    dens0 <- y
    y <- dens0$data
  } else
  {
    y <- as.matrix(y)
    dens0 <- gmm_basket_shot_chart(y, ...)
  }
  
  pred <- gmm_basket_shot_chart_predict(dens1, dens0)
  df <- data.table(x = pred$data[,1], y = pred$data[,2])
  df[, Area := apply(df[,.(x,y)], 1, basket_court_point_area)]
  df[, Shot := rep(c("made", "missed"), c(dens1$n, dens0$n))]
  df[, Probs := pred$Probs]
  tab <- df[order(Area), .(Attempts = length(Shot),
                           ObsProp = mean(Shot == "made"),
                           AveProb = mean(Probs, na.rm = TRUE)),
            by = Area]
  tab[, Error := AveProb - ObsProp]

  # Set default labels if not provided
  if(is.null(labels)) 
    labels <- set(nrow(tab))
  
  nce <- with(tab, weighted.mean(abs(AveProb - ObsProp), Attempts))

  plot <- ggplot(data = tab, aes(x = AveProb, 
                                 y = ObsProp, 
                                 color = Area, 
                                 shape = Area)) +
    # Add reference line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    # Add points
    geom_point(aes(size = Attempts)) +
    # Set scales
    scale_color_manual(labels = palette$name,
                       values = palette$color) +
    scale_shape_manual(labels = palette$name, 
                       values = seq(0, length(tab$Area)-1)) +
    # Make plot square with equal scales
    coord_equal(xlim = c(0,1), 
                ylim = range(c(0,1,tab$Error))*c(2,1), 
                expand = FALSE) +
    # Customize appearance
    guides(color = guide_legend(title = NULL),
           shape = guide_legend(title = NULL),
           size  = "none") +
    labs(subtitle = sprintf("NCE: %.4f", nce),
         x = "Predicted probability", 
         y = "Observed proportion") +
    theme_minimal() +
    theme(plot.background =  element_rect(color = theme_minimal()$rect$fill))
  
  # Return results
  out <- list(tab = setalloccol(tab),
              nce = nce, 
              plot = plot)
  return(out)
}

