#' Radius of influence
#'
#' @description Radius of influence for a stationary groundwater well in a
#' freatic aquifer with net recharge. Reference: De Smedt, 2007
#'
#' \deqn{\sqrt{a + b}}
#'
#' @param Q abstraction (L^3/T)
#' @param Rech recharge (L/T)
#'
#' @return radius of influence (L)
#' @export
#'

r_freat_steady <- function(Q, Rech) {

  r <- (Q / (pi * Rech))^0.5
  return(r)
}
