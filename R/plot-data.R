

getAes <- function(color=TRUE, shape=FALSE) {
  ifelse(color, 
    ifelse( shape, 
      aes(x = distance, y = price, color=year, shape=year), 
      aes(x = distance, y = price, color=year)
    ),
    aes(x = distance, y = price)
  )
}

#' @export
priceFctDist <- function(d, plotTitle='By Brand') {
  ggplot(d, aes(x = distance, y = price, color=year)) + geom_point() + ggtitle(plotTitle)
}

#' Get a facet plot by brand, price func(distance), 
#' 
#' Get a facet plot by brand, price func(distance), 
#' 
#' @export
byBrand <- function(d, plotTitle='By Brand') {
# byBrand <- function(d, color=TRUE, shape=FALSE, plotTitle='By Brand') {
  # aest <- getAes(color=color, shape=shape) does not work. 
  priceFctDist(d, plotTitle) + facet_wrap( ~ brand)
}

#' Get a facet plot by model, price func(distance), 
#' 
#' Get a facet plot by model, price func(distance), 
#' 
#' @export
byModel <- function(d, plotTitle='By Model') {
# byModel <- function(d, color=TRUE, shape=FALSE, plotTitle='By Model') {
  # aest <- getAes(color=color, shape=shape) does not work. 
  priceFctDist(d, plotTitle) + facet_wrap( ~ model)
}

#' @export
# defaultPlotSetup <- function( plotTitle = 'no title' ) {
#   geom_point() + ggtitle(plotTitle)
# }