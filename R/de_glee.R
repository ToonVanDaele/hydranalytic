#' De Glee  (reference to be added)
#'
#' @description de Glee semi-confined aquifer with stationary flow
#'
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal conductivity (L/T)
#' @param r distance to well (L)
#' @param Dc saturated thickness confining aquifer (L)
#' @param Kv vertical conductivity of confining aquifer (L/T)
#'
#' @export
#'
#' @return s drawdown (L)
#'
#-------------------------------------------------------------------------------
deglee <- function(Q, D, Kh, r, Dc, Kv) {

  require(assertthat)
  assert_that(all(Kh > 0 & D > 0 & Kh > 0 & Dc > 0 & Kv > 0))
  assert_that(all(Q >= 0 & r >= 0))

  L <- sqrt(Kh * D * Dc / Kv)

  s <- (Q / 2 * pi * Kh * D) * besselI(x = r / L, nu = 0)

  return(s)
}
