#' Error function erf
#'
#' @description Erf & Erfc - Error function and complementary error function
#' (based on last lines in ?pnorm help file)
#' (see Abramowitz and Stegun 29.2.29) and the so-called
#' 'complementary error function'
#'
#' @param x x (-)
#'
#' @importFrom stats pnorm
#'
#' @return erf erf (-)

require(stats)
erf <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
  }
