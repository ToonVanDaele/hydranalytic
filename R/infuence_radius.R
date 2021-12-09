#' Radius of equal recharge
#'
#' @description Radius of a circle for a stationary groundwater well were
#' extraction equals recharge. Reference: De Smedt, 2007
#'
#' \deqn{\sqrt{a + b}}
#'
#' @param Q abstraction (L^3/T)
#' @param Rech recharge (L/T)
#'
#' @return radius (L)
#' @export
#'

freat_equal_r <- function(Q, Rech) {

  require(assertthat)

  assert_that(all(Q >= 0))
  assert_that(all(Rech > 0))

  r <- (Q / (pi * Rech))^0.5
  return(r)
}
