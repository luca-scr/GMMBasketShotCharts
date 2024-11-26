# GMMBasketShotCharts

<!-- badges: start -->
<!-- badges: end -->

R package implementing a model-based approach using Gaussian mixtures for bounded data to estimate and visualize basketball shot charts.

The method is described in the paper:

Scrucca L., Karlis D. (2024) A model-based approach to shot charts estimation in basketball. Under review.
arXiv pre-print available at https://arxiv.org/abs/2405.01182

## Installation

You can install the development version of the package using the following code:

- Install/update `devtools` package to the latest version
	  
    ``` r
    install.packages("devtools")
    ```

- Install/update `GMMBasketShotCharts` package

    ``` r
    devtools::install_github("luca-scr/GMMBasketShotCharts")
    ```

## Example

Code to reproduce the analyses presented in Scrucca and Karlis (2024):

``` r
library(GMMBasketShotCharts)

plot_basket_court()
plot_basket_court_offensive_areas()

# Stephen Curry -------------------------------------------------------

data(stephen_curry, package = "GMMBasketShotCharts")
data = stephen_curry$data[, .(Shot, x, y)]

dens1 = gmm_basket_shot_chart(data[Shot == "made",])
summary(dens1)
plot(dens1, prob = c(0.1, 0.25, 0.5, 0.75, 0.9), palette = "OrRd") + 
  geom_point(data = as.data.table(dens1$data), 
             aes(x = x, y = y),
             pch = 1, col = "firebrick4")

dens0 = gmm_basket_shot_chart(data[Shot == "missed",])
summary(dens0)
plot(dens0, prob = c(0.1, 0.25, 0.5, 0.75, 0.9), palette = "PuBu") + 
  geom_point(data = as.data.table(dens1$data), 
             aes(x = x, y = y),
             pch = 1, col = "dodgerblue4")

pred = gmm_basket_shot_chart_predict(dens1, dens0, newdata = data[,.(x,y)])
data[, Probs := pred$Probs]
data[, ExpPoints := pred$ExpPoints]

cols = tableau_color_pal(palette = "Red-Blue Diverging", 
                         type = "ordered-diverging", 
                         direction = -1)(7)
ggplot(data) + 
  plot_basket_court(theme = "light", add = TRUE) +
  stat_summary_hex(aes(x = x, y = y, z = Probs),
                   binwidth = 1) +
  scale_fill_gradientn(name = NULL, 
                       colors = cols,
                       guide = "colorbar",
                       values = scales::rescale(c(0,0.1,0.2,0.3,0.35,0.4,0.5,1),
                                                from = c(0,1)),
                       limits = c(0,1)) +
  labs(title = "Shot chart scoring probability")

cols2 = tableau_color_pal(palette = "Green-Blue Diverging", 
                          type = "ordered-diverging", 
                          direction = -1)(7)
ggplot(data) + 
  plot_basket_court(theme = "light", add = TRUE) +
  stat_summary_hex(aes(x = x, y = y, z = ExpPoints),
                   binwidth = 1) +
  scale_fill_gradientn(name = NULL, 
                       colors = cols2,
                       values = scales::rescale(c(0,0.25,0.5,1,1.25,1.5,3),
                                                from = c(0, 3)),
                       limits = c(0, 3)) +
  labs(title = "Shot chart expected points")

gmm_basket_shot_chart_calibration(dens1, dens0)


# Joel Embiid ---------------------------------------------------------

data(joel_embiid, package = "GMMBasketShotCharts")
data = joel_embiid$data[, .(Shot, x, y)]

dens1 = gmm_basket_shot_chart(data[Shot == "made",])
summary(dens1)
plot(dens1, prob = c(0.1, 0.25, 0.5, 0.75, 0.9), palette = "OrRd") + 
  geom_point(data = as.data.table(dens1$data), 
             aes(x = x, y = y),
             pch = 1, col = "firebrick4")

dens0 = gmm_basket_shot_chart(data[Shot == "missed",])
summary(dens0)
plot(dens0, prob = c(0.1, 0.25, 0.5, 0.75, 0.9), palette = "PuBu") + 
  geom_point(data = as.data.table(dens1$data), 
             aes(x = x, y = y),
             pch = 1, col = "dodgerblue4")


pred = gmm_basket_shot_chart_predict(dens1, dens0, newdata = data[,.(x,y)])
data[, Probs := pred$Probs]
data[, ExpPoints := pred$ExpPoints]

cols = tableau_color_pal(palette = "Red-Blue Diverging", 
                         type = "ordered-diverging", 
                         direction = -1)(7)
ggplot(data) + 
  plot_basket_court(theme = "light", add = TRUE) +
  stat_summary_hex(aes(x = x, y = y, z = Probs),
                   binwidth = 1) +
  scale_fill_gradientn(name = NULL, 
                       colors = cols,
                       guide = "colorbar",
                       values = scales::rescale(c(0,0.1,0.2,0.3,0.35,0.4,0.5,1),
                                                from = c(0,1)),
                       limits = c(0,1)) +
  labs(title = "Shot chart scoring probability")

cols2 = tableau_color_pal(palette = "Green-Blue Diverging", 
                          type = "ordered-diverging", 
                          direction = -1)(7)
ggplot(data) + 
  plot_basket_court(theme = "light", add = TRUE) +
  stat_summary_hex(aes(x = x, y = y, z = ExpPoints),
                   binwidth = 1) +
  scale_fill_gradientn(name = NULL, 
                       colors = cols2,
                       values = scales::rescale(c(0,0.25,0.5,1,1.25,1.5,3),
                                                from = c(0, 3)),
                       limits = c(0, 3)) +
  labs(title = "Shot chart expected points")

gmm_basket_shot_chart_calibration(dens1, dens0)
```

