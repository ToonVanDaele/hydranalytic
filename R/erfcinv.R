#' erfcinv - inverse complementary error function
#'
#' @description erfcinv - inverse complementary error function
#' (based on last lines in ?pnorm help file)
#' (see Abramowitz and Stegun 29.2.29) and the so-called 'complementary error
#' function'
#'
#' @param x x (-)
#'
#' @importFrom stats qnorm
#'
#' @return erfcinv (-)

erfcinv <- function(x) qnorm(x / 2, lower.tail = FALSE) / sqrt(2)
