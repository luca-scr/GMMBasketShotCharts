#' Gaussian mixture model for basketball shot charts estimation
#' 
#' @description
#' 
#' An R package implementing a model-based approach using Gaussian mixtures 
#' for bounded data to estimate and visualize basketball shot charts
#' (Scrucca & Karlis, 2025).
#' 
#' @seealso 
#' [densityMclustBounded()] for density estimation of bounded data.
#' 
#' @references 
#' 
#' Scrucca L., Karlis D. (2025) A model-based approach to shot charts estimation
#'   in basketball. Computational Statistics. 
#'   https://doi.org/10.1007/s00180-025-01599-1
#'   
#' @keywords internal
"_PACKAGE"

#' @import mclust mclustAddons ggplot2 data.table
#' @importFrom utils menu packageVersion
#' @importFrom stats na.omit predict 
#' @importFrom Rcpp sourceCpp
#' 
#' @useDynLib GMMBasketShotCharts, .registration = TRUE
NULL
  
#' Stephen Curry data
#' 
#' Shooting data for Stephen Curry, 2022-23 Regular Season.
#' 
#' @name stephen_curry
#' @docType data
#' @format A list of two components:
#' \describe{
#'   \item{data}{a `data.table` containing player's features.} 
#'   \item{player}{a `data.table` containing player's info.} 
#' }
#' @source 
#' Data obtained using the R package hoopR (Gilani, 2023) from ESPN analytics 
#' https://www.espn.com/nba/.
#'
#' @keywords datasets
NULL

#' Joel Embiid data
#' 
#' Shooting data for Joel Embiid, 2022-23 Regular Season.
#' 
#' @name joel_embiid
#' @docType data
#' @format A list of two components:
#' \describe{
#'   \item{data}{a `data.table` containing player's features.} 
#'   \item{player}{a `data.table` containing player's info.} 
#' }
#' @source 
#' Data obtained using the R package hoopR (Gilani, 2023) from ESPN analytics 
#' https://www.espn.com/nba/.
#'
#' @keywords datasets
NULL


