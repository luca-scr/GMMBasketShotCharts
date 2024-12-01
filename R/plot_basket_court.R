# Basketball helf-court size and coordinates
basket_court_nba = list(
  width = 50,
  height = 94/2,
  key_height = 19,
  inner_key_width = 12,
  outer_key_width = 16,
  backboard_width = 6,
  backboard_offset = 4,
  neck_length = 0.5,
  hoop_radius = 0.75,
  hoop_center_y = 4 + 0.5 + 0.75, # backboard_offset + neck_length + hoop_radius
  three_point_radius = 23.75,
  three_point_side_radius = 22,
  three_point_side_height = 14)

#' @rdname plot_basket_court
#' @export
# Basketball court light and dark themes
basket_court_theme = list(
  "light" = list(court = "#ffffff",
                 lines = "#333333",
                 linewidth = 1,
                 text = "#111111",
                 made = "#E63946",
                 missed = "#2297E6"),
  "dark" = list(court = "#111111",
                lines = "#ffffff",
                linewidth = 1,
                text = "#cccccc",
                made = "#E63946",
                missed = "#2297E6",
                hex_border_size = 0,
                hex_border_color = "#000000") 
)

#' @rdname plot_basket_court
#' @export
# Basketball court offensive areas theme
basket_offensive_areas_theme = list(
  name = c("Paint", "Restricted area", 
           "Midrange left", "Midrange center", "Midrange right", 
           "Three-point left corner", "Three-point left", "Three-point center", 
           "Three-point right", "Three-point right corner"),
  area = c("paint", "restricted", 
           "midrange_left", "midrange_center", "midrange_right",
           "three_point_left_corner", "three_point_left", "three_point_center",
           "three_point_right", "three_point_right_corner"),
  color = c("#FF9933", # Light Brown
            "#CC6600", # Medium Brown
            "#A8DADC", # Light Blue
            "#457B9D", # Deep Blue
            "#B0C4DE", # Light Steel Blue
            "#2A9D8F", # Light Teal Green
            "#00796B", # Teal Green
            "#015D64", # Dark Teal Green
            "#5BA3A3", # Pale Slate
            "#ACC5BC"  # Grey-Blue
           )
)

#' @name plot_basket_court
#' @aliases plot_basket_court
#' 
#' @title Plot basket half-court 
#' 
#' @description 
#' TODO
#'
#' @param theme TODO
#' @param add TODO
#' @param use_short_three TODO
#'
#' @return A ggplot object.
#' 
#' @examples
#' plot_basket_court()
#' plot_basket_court(theme = "dark")
#' 
#' basket_court_theme
#' basket_offensive_areas_theme
#' 
#' @export

plot_basket_court = function(theme = c("light", "dark"), 
                             add = FALSE,
                             use_short_three = FALSE) 
{
  # Based on code from https://github.com/toddwschneider/ballr
  theme = match.arg(theme)
  court_theme = basket_court_theme[[theme]]
  
  court = basket_court_nba
  if(use_short_three) 
  {
    court$three_point_radius = 22
    court$three_point_side_height = 0
  }
  
  circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) 
  {
    angles = seq(0, 2 * pi, length.out = npoints)
    return(data.table(x = center[1] + radius * cos(angles),
                      y = center[2] + radius * sin(angles)))
  }

  court_points = data.table(x = court$width * c(1,1,-1,-1,1)/2,
                            y = court$height * c(1, 0, 0, 1, 1),
                            desc = "perimeter")

  court_points = rbind(court_points, 
                       data.table(x = court$outer_key_width * c(1,1,-1,-1)/2,
                                  y = court$key_height * c(0,1,1,0),
                                  desc = "outer_key") )

  court_points = rbind(court_points, 
                       data.table(x = court$backboard_width * c(-1,1)/2,
                                  y = court$backboard_offset * c(1,1),
                                  desc = "backboard") )

  court_points = rbind(court_points,
                       data.table(x = c(0, 0), 
                                  y = court$backboard_offset + 
                                      c(0,1)*court$neck_length,
                                  desc = "neck") )

  foul_circle = circle_points(center = c(0, court$key_height), 
                              radius = court$inner_key_width/2)

  foul_circle_top = data.table(foul_circle[y > court$key_height,], 
                               desc = "foul_circle_top")
  
  foul_circle_bottom = foul_circle[y < court$key_height,]
  foul_circle_bottom[, angle := atan((y - court$key_height) / x) * 180 / pi]
  foul_circle_bottom[, angle_group := floor((angle - 5.625) / 11.25)]
  foul_circle_bottom[, desc := paste0("foul_circle_bottom_", angle_group)]
  foul_circle_bottom = foul_circle_bottom[angle_group %% 2 == 0, 
                                          c("x", "y", "desc")]

  hoop = cbind(circle_points(center = c(0, court$hoop_center_y), 
                             radius = court$hoop_radius),
               desc = "hoop")

  restricted = cbind(circle_points(center = c(0, court$hoop_center_y), 
                                   radius = 4),
                     desc = "restricted")
  restricted = restricted[y >= court$hoop_center_y,]

  three_point_circle = circle_points(center = c(0, court$hoop_center_y), 
                                     radius = court$three_point_radius)
  three_point_circle = three_point_circle[y >= court$three_point_side_height & 
                                          y >= court$hoop_center_y,]

  three_point_line = data.table(
    x = c(court$three_point_side_radius, 
          court$three_point_side_radius, 
          three_point_circle$x, 
          -court$three_point_side_radius, 
          -court$three_point_side_radius),
    y = c(0, court$three_point_side_height, 
          three_point_circle$y,
          court$three_point_side_height, 0),
    desc = "three_point_line")
  
  court_points = rbind(court_points,
                       foul_circle_top,
                       foul_circle_bottom,
                       hoop,
                       restricted,
                       three_point_line)
  # make (0,0) the center court (basket at bottom)
  # court_points[, y := y - 47]
  # make (0,0) the center court (basket at top) 
  court_points[, y := court$height - y]

  court = list(
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = court_theme$lines,
              linewidth = court_theme$linewidth),
    coord_fixed(xlim = range(court_points$x),
                ylim = range(court_points$y)),
    theme_minimal(),
    theme(text = element_text(color = court_theme$text),
          plot.background = element_rect(fill = court_theme$court, 
                                         color = court_theme$court),
          panel.background = element_rect(fill = court_theme$court, 
                                          color = court_theme$court),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill = court_theme$court, 
                                           color = court_theme$court),
          legend.margin = ggplot2::margin(-1, 0, 0, 0, unit = "lines"),
          legend.position = "right",
          legend.key = element_blank(),
          legend.text = element_text(size = rel(0.9)),
          legend.title = element_text(size = rel(0.9)) )
  )
  
  if(add) 
    return(court)
  else 
  { gg = ggplot() + court
    return(gg) }
}

#' @name plot_basket_court_offensive_areas
#' @aliases plot_basket_court_offensive_areas
#' 
#' @title Plot basket half-court offensive areas
#' 
#' @description 
#' TODO
#'
#' @param theme TODO
#' @param add TODO
#' @param use_short_three TODO
#'
#' @return A ggplot object.
#' 
#' @examples
#' plot_basket_court_offensive_areas()
#' plot_basket_court_offensive_areas(theme = "dark")
#' 
#' @export

plot_basket_court_offensive_areas <- function(theme = "light",
                                              use_short_three = FALSE, 
                                              palette = basket_offensive_areas_theme,
                                              ...)
{
  
  xyoff = basket_court_offensive_areas()
  xyoff[, area := factor(area, levels = palette$area)]

  basket_court = plot_basket_court(theme = theme) +
    scale_x_continuous(breaks = seq(-25,25,by=5)) +
    scale_y_continuous(breaks = c(seq(0,40,by=10), 47)) +
    theme(axis.line = element_line(color = "grey20"),
          axis.text = element_text(size = 10),
          axis.ticks = element_line(colour = "grey20"))
  basket_court +
    geom_polygon(data = xyoff, aes(x, y, fill = area)) +
    plot_basket_court(theme = "light", add = TRUE) +
    scale_fill_manual(labels = palette$name, values = palette$color) +
    guides(fill = guide_legend(title = NULL))
}


# R version, now Rcpp version is used
# point_in_polygon <- function(point, polygon, boundary = TRUE, ...) 
# {
# # Check if a point lies inside a polygon. 
# # The ray-casting algorithm is used, which involves drawing a horizontal ray 
# # from the point in question and counting how many times it intersects the 
# # edges of the polygon. A point is considered strictly inside if the number of
# # intersections is odd. 
# # 
# # Arguments:
# # point: vector of (x,y) coordinates
# # polygon: two-column matrix of (x,y) coordinates for the (closed) polygon
# # boundary: logical, if TRUE allows the point to lie on the boundary
#   
#   point <- as.vector(unlist(point))[1:2]
#   polygon <- as.matrix(polygon[,1:2])
#   n <- nrow(polygon)
#   # Unpack point coordinates
#   x <- point[1]
#   y <- point[2]
#   
#   # Initialize intersection count
#   intersections <- 0
#   
#   # Function to check if a point is on a line segment
#   is_on_segment <- function(px, py, x1, y1, x2, y2) 
#   {
#     # Check for collinearity and that the point lies within the bounds of the segment
#     collinear <- (y2 - y1) * (px - x1) == (py - y1) * (x2 - x1)
#     within_bounds <- px >= min(x1, x2) && px <= max(x1, x2) && py >= min(y1, y2) && py <= max(y1, y2)
#     return(collinear && within_bounds)
#   }
# 	 
#   # Loop through each edge of the polygon
#   for (i in 1:n) 
#   {
#     # Get coordinates of the current edge
#     x1 <- polygon[i, 1]
#     y1 <- polygon[i, 2]
#     x2 <- polygon[(i %% n) + 1, 1]
#     y2 <- polygon[(i %% n) + 1, 2]
# 
#     # Check if the point lies exactly on the edge
# 		if (is_on_segment(x, y, x1, y1, x2, y2)) 
# 		{
# 			if(boundary) return(TRUE) else return(FALSE) 
# 		}
#     
#     # Ray-casting check for intersection
#     if ((y > min(y1, y2)) && (y <= max(y1, y2)) && (x <= max(x1, x2))) 
#     {
#       # Find x-intersection of the edge with the horizontal ray
#       x_intersection <- (y - y1) * (x2 - x1) / (y2 - y1) + x1
#       if (x1 == x2 || x <= x_intersection) 
#       {
#         intersections <- intersections + 1
#       }
#     }
#   }
#   
#   # The point is inside if the number of intersections is odd
#   return(intersections %% 2 == 1)
# }


#' @rdname plot_basket_court
#' @export

basket_court_offensive_areas <- function(use_short_three = FALSE, ...)
{
# Offensive basketball court areas.
# Implemented: restricted, paint, midrange_left, midrange_center, midrange_right,
#              three_point_left_corner, three_point_left, three_point_center, 
#              three_point_right, three_point_right_corner

  stopifnot(is.logical(use_short_three))
  
  court <- basket_court_nba
  if(use_short_three) 
  {
    court$three_point_radius <- 22
    court$three_point_side_height <- 0
  }
  
  ## used this basketball court to check the coordinates of areas:
  # basket_court = plot_basket_court(theme = "light") +
  #   scale_x_continuous(breaks = seq(-25,25,by=5)) +
  #   scale_y_continuous(breaks = c(seq(0,40,by=10), 47)) +
  #   theme(axis.line = element_line(color = "grey20"),
  #         axis.text = element_text(size = 10),
  #         axis.ticks = element_line(colour = "grey20"))

  circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) 
  {
    angles = seq(0, 2 * pi, length.out = npoints)
    return(data.table(x = center[1] + radius * cos(angles),
                      y = center[2] + radius * sin(angles)))
  }

  restricted <- circle_points(center = c(0, court$hoop_center_y), radius = 4)
  restricted <- restricted[y >= court$hoop_center_y,]
  restricted[, y := court$height - y]
  restricted <- rbind(list(4, court$height-court$backboard_offset),
                      restricted,
                      list(-4, court$height-court$backboard_offset),
                      list(4, court$height-court$backboard_offset))
  restricted <- cbind(restricted, area = "restricted")
  
  paint <- data.table(x = court$outer_key_width * c(1,1,-1,-1)/2,
                      y = court$height - court$key_height * c(0,1,1,0),
                      area = "paint")

  three_point_circle <- circle_points(center = c(0, court$hoop_center_y), 
                                      radius = court$three_point_radius)
  inc <- (three_point_circle$y > court$three_point_side_height & 
          three_point_circle$y > court$hoop_center_y)
  three_point_circle <- three_point_circle[inc,,drop=FALSE]
  
  three_point_area <- data.table(x = c(court$width/2,
                                       court$three_point_side_radius, 
                                       court$three_point_side_radius, 
                                       unlist(three_point_circle[,1]), 
                                       rep(-court$three_point_side_radius,2),
                                       rep(-court$width/2,2),
                                       rep(court$width/2,2)),
                                 y = c(court$height - c(0,0,
                                                        court$three_point_side_height, 
                                                        three_point_circle$y, 
                                                        court$three_point_side_height, 
                                                        0, 0),
                                       0, 0, court$height),
                                 area = "three")
  # basket_court + geom_path(data = three_point_area, aes(x, y), col = 2)
  
  three_point_line = three_point_area[x >= -court$three_point_side_radius & 
                                      x <= court$three_point_side_radius & 
                                      y > 10 & y <= 47]
  three_point_line[, area := NULL]
  three_point_line = rbind(three_point_line, list(22, 47))
  # basket_court + geom_path(data = three_point_line, aes(x, y), col = 2)

  three_point_left_corner <- data.table(x = c(-court$width/2,
                                              -court$three_point_side_radius, 
                                              -court$three_point_side_radius,
                                              -court$width/2),
                                        y = court$height - 
                                          c(rep(0,2),
                                            rep(court$three_point_side_height,2)),
                                        area = "three_point_left_corner")   
  # basket_court + geom_path(data = three_point_left_corner, aes(x,y), col = 2)

  three_point_right_corner <- data.table(x = c(court$width/2,
                                               rep(court$three_point_side_radius,2),
                                               court$width/2),
                                         y = court$height - 
                                           c(rep(0,2), 
                                             rep(court$three_point_side_height,2)),
                                         area = "three_point_right_corner")
  # basket_court + geom_path(data = three_point_right_corner, aes(x,y), col = 2)
  
  three_point_left <- data.table(x = c(rep(-court$width/2,2), -12.07,
                                       three_point_area[x > -court$three_point_side_radius & x < -12.07]$x,
                                       -court$three_point_side_radius, -court$width/2),
                                 y = c(court$height - court$three_point_side_height, 
                                       0, 21.3,
                                       three_point_area[x > -court$three_point_side_radius & x < -12.07]$y,
                                       rep(court$height - court$three_point_side_height,2)),
                                 area = "three_point_left")
  # basket_court + geom_path(data = three_point_left, aes(x,y), col = 2)
  
  three_point_right <- data.table(x = c(court$width/2, court$width/2, 12.07,
                                        rev(three_point_area[x > 12.07 & x < court$three_point_side_radius]$x),
                                        court$three_point_side_radius, court$width/2),
                                  y = c(court$height - court$three_point_side_height, 0, 21.3, 
                                        rev(three_point_area[x > 12.07 & x < court$three_point_side_radius]$y),
                                        rep(court$height - court$three_point_side_height,2)),
                                  area = "three_point_right")
  # basket_court + geom_path(data = three_point_right, aes(x,y), col = 2)
  
  three_point_center <- data.table(x = c(-court$width/2, -12.07, 
                                         -three_point_area[x > -12.07 & x < 12.07]$x, 
                                         12.07, court$width/2, -court$width/2), 
                                   y = c(0, 21.3,
                                         three_point_area[x > -12.07 & x < 12.07]$y,
                                         21.3, 0, 0),
                                   area = "three_point_center")
  # basket_court +
  #   geom_segment(aes(x = -8, xend = -25, y = 28, yend = 0)) +
  #   geom_segment(aes(x = 8, xend = 25, y = 28, yend = 0)) +
  #   geom_path(data = three_point_center, aes(x, y), col = 2)
  
  midrange <- data.table(x = c(court$three_point_side_radius, 
                               court$three_point_side_radius, 
                               three_point_circle$x, 
                               -court$three_point_side_radius,
                               -court$three_point_side_radius,
                               court$outer_key_width * c(-1,-1,1,1)/2,
                               court$three_point_side_radius),
                         y = c(court$height - 
                                 c(0, court$three_point_side_height, 
                                   three_point_circle$y, 
                                   court$three_point_side_height, 0, 0,
                                   court$key_height * c(1,1),
                                   0, 0)),
                         area = "midrange")
  
  midrange_left <- data.table(x = c(rep(-court$three_point_side_radius, 2),
                                    rev(three_point_line[x > -court$three_point_side_radius & x < -12.07]$x),
                                    -12.07, rep(-court$outer_key_width/2, 2),
                                    -court$three_point_side_radius),
                              y = c(court$height - c(0, court$three_point_side_height),
                                    rev(three_point_line[x > -court$three_point_side_radius & x < -12.07]$y),
                                    21.3, court$inner_key_width + court$outer_key_width,
                                    rep(court$height, 2)),
                              area = "midrange_left")
  # basket_court + geom_path(data = midrange_left, aes(x, y), col = 2)
  
  midrange_center <- data.table(x = c(-court$outer_key_width/2, -12.07,
                                      rev(three_point_line[x > -12.07 & x < 12.07]$x),
                                      12.07, court$outer_key_width/2*c(1,-1)),
                                y = c(court$inner_key_width + court$outer_key_width, 21.3,
                                      rev(three_point_line[x > -12.07 & x < 12.07]$y),
                                      21.3, rep(court$inner_key_width + court$outer_key_width,2)),
                                area = "midrange_center")
  # basket_court + geom_path(data = midrange_center, aes(x, y), col = 2)

  midrange_right <- data.table(x = c(rep(court$three_point_side_radius, 2),
                                     three_point_line[x > 12.07 & x < court$three_point_side_radius]$x,
                                     12.07, rep(court$outer_key_width/2, 2),
                                     court$three_point_side_radius),
                               y = c(court$height - c(0, court$three_point_side_height),
                                     three_point_line[x > 12.07 & x < court$three_point_side_radius]$y,
                                     21.3, court$inner_key_width + court$outer_key_width,
                                     rep(court$height, 2)),
                               area = "midrange_right")
  # basket_court + geom_path(data = midrange_right, aes(x, y), col = 2)
  
  out <- rbind(restricted, paint,
               midrange_left, midrange_center, midrange_right,
               three_point_left_corner, three_point_left,
               three_point_center, 
               three_point_right, three_point_right_corner)
  return(out)
}

#' @rdname plot_basket_court
#' @export

# old
# basket_court_point_area <- function(point, ...)
# {
# # Given a (x,y) data point return the corresponding offensive area as defined in
# # basket_court_offensive_areas() 
# 
#   point <- as.vector(unlist(point))
#   xyoff <- basket_court_offensive_areas(...)
#   uarea <- unique(xyoff$area)
#   area <- sapply(uarea, function(a) 
#                  point_in_polygon(point, xyoff[area == a], ...))
#   area <- factor(uarea[which(area)[1]], levels = uarea)
#   return(area)
# }

basket_court_point_area <- function(point, ...)
{
  # Given a (x,y) data point return the corresponding offensive area 
  # as defined in basket_court_offensive_areas() 
  point <- as.vector(unlist(point))
  xyoff <- basket_court_offensive_areas(...)
  xy <- as.matrix(xyoff[, c("x", "y")])
  uarea <- unique(xyoff$area)
  area <- sapply(uarea, function(a) 
                 point_in_polygon(point, xy[xyoff$area == a,], ...))
  area <- factor(uarea[which(area)[1]], levels = uarea)
  return(area)
}


#' @rdname plot_basket_court
#' @export

# old
# three_point_shot <- function(point, use_short_three = FALSE, ...)
# {
# # Return TRUE if point (x,y) coordinates correspond to a 3-point shot
#   point <- as.vector(unlist(point))
#   xyoff <- basket_court_offensive_areas(use_short_three = use_short_three, ...)
#   uarea <- unique(xyoff$area)
#   area <- sapply(uarea, function(a) 
#                  point_in_polygon(point, xyoff[area == a], ...))
#   area <- uarea[which(area)]
#   all(grepl("three", area))
# }
  
three_point_shot <- function(point, ...)
{
  # Given a (x,y) data point return TRUE if point (x,y) coordinates 
  # correspond to a 3-point shot 
  point <- as.vector(unlist(point))
  xyoff <- basket_court_offensive_areas(...)
  xy <- as.matrix(xyoff[, c("x", "y")])
  uarea <- unique(xyoff$area)
  area <- sapply(uarea, function(a) 
                 point_in_polygon(point, xy[xyoff$area == a,], ...))
  area <- uarea[which(area)]
  all(grepl("three", area))
}
  