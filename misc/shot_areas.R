library(mixbasketchart)

palette_offensive_areas = list(
  name = c("Paint", "Restricted area", "Midrange left", "Midrange center", "Midrange right", "Three-point left corner", "Three-point left", "Three-point center", "Three-point right", "Three-point right corner"),
  area = c("paint", "restricted", "midrange_left", "midrange_center", "midrange_right", "three_point_left_corner", "three_point_left", "three_point_center", "three_point_right", "three_point_right_corner"),
  color = c("#F1A208", # Orange
            "#E63946", # Soft Red
            "#A8DADC", # Light Blue
            "#457B9D", # Deep Blue
            "#B0C4DE", # Light Steel Blue
            "#2A9D8F", # Light Teal Green
            "#00796B", # Teal Green
            "#015D64", # Dark Teal Green
            "#5BA3A3", # Pale Slate
            "#ACC5BC" # Grey-Blue
  )
)

basket_court = plot_basket_court(theme = "light") +
  scale_x_continuous(breaks = seq(-25,25,by=5)) +
  scale_y_continuous(breaks = c(seq(0,40,by=10), 47)) +
  theme(axis.line = element_line(color = "grey20"),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(colour = "grey20"))
basket_court

xyoff = basket_court_offensive_areas()
xyoff[, area := factor(area, levels = palette_offensive_areas$area)]
basket_court +
  geom_polygon(data = xyoff, aes(x, y, fill = area)) +
  plot_basket_court(theme = "light", add = TRUE) +
  scale_fill_manual(values = palette_offensive_areas$color,
                    labels = palette_offensive_areas$name) +
  guides(fill = guide_legend(title = NULL))

#----------------------------------------------------------------------

basket_court = plot_basket_court(theme = "light") +
  geom_segment(aes(x = 8, xend = 25, y = 28, yend = 0)) +
  geom_segment(aes(x = -8, xend = -25, y = 28, yend = 0)) +
  geom_segment(aes(x = -22, xend = -25, y = 33, yend = 33)) +
  geom_segment(aes(x = 22, xend = 25, y = 33, yend = 33)) +
  scale_x_continuous(breaks = seq(-25,25,by=5)) +
  scale_y_continuous(breaks = c(seq(0,40,by=10), 47)) +
  theme(axis.line = element_line(color = "grey20"),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(colour = "grey20"))
basket_court

# random points
xy_point = data.frame(x = runif(1, -24.5, 24.5), y = runif(1, 1, 46.5))
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

# selected points
xy_point = data.frame(x = 0, y = 30)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

xy_point = data.frame(x = 0, y = 18)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

xy_point = data.frame(x = -22, y = 33)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

xy_point = data.frame(x = 22, y = 33)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

xy_point = data.frame(x = -22, y = 35)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

xy_point = data.frame(x = 22, y = 35)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

xy_point = data.frame(x = 22, y = 47)
basket_court + 
  geom_point(data = xy_point, aes(x, y), col = 2, size = 3) +
  labs(subtitle = paste(basket_court_point_area(xy_point), 
                        ifelse(three_point_shot(xy_point), "[3pt]", "[2pt]")))

