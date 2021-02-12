#' Edelman - flow
#'
#' @description Edelman horizontal flow in a cross section Reference: xxx
#'
#' @param t time (T)
#' @param Kh horizontal transmissivity (L/T)
#' @param D saturated thickness (level in rest) (L)
#' @param s drawdown at the border of the cross section (L)
#'
#' @export
#'
#' @return Q abstraction (L^3/T)
#-----------------------------------------------------------------------------------
Edelman_Q <- function(t, Kh, D, S, s) {


  Q <- s * sqrt((S * Kh * D) / (pi * t))

  return (Q)
}
