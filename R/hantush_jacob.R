#' Hantush  (reference to be added)
#'
#' @description Hantush semi-confined aquifer with stationary flow
#'
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal hydraulic conductivity (L/T)
#' @param r distance to well (L)
#' @param Dc saturated thickness confining aquifer (L)
#' @param c vertical hydraulic conductivity of confining aquifer (L/T)
#'
#' @return s drawdown (L)
#'
#' @examples
#' DeGlee(50, 20, 2, 30, 10, 0.2)
#'
#' @export
#'
Hantush <- function(Q, D, Kh, r, Dc, c) {

  s <- (Q / 4 * pi * Kh * D) * besselI(r / sqrt(Kh * D * c))
  return(s)

}
