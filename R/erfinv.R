#' erfinv - inverse error function
#'
#' @description erfinv - inverse error function
#' (based on last lines in ?pnorm help file)
#' (see Abramowitz and Stegun 29.2.29) and the so-called 'complementary error
#' function'

#'
#' @param x x (-)
#'
#' @return erfinv (-)

erfinv <- function(x) qnorm((1 + x) / 2) / sqrt(2)
