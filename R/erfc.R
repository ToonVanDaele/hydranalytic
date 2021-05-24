#' Complementary error function erfc
#'
#' @description Erfc - Error function and complementary error function
#' (based on last lines in ?pnorm help file)
#' (see Abramowitz and Stegun 29.2.29) and the so-called 'complementary error
#' function'
#'
#' @param x x (-)
#'
#' @importFrom stats pnorm
#'
#' @return erfc erfc (-)

erfc <- function(x) {
    2 * pnorm(x * sqrt(2), lower.tail = FALSE)
  }
