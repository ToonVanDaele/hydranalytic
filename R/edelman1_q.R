#' Edelman - flow
#'
#' @description Edelman horizontal flow in a cross section Reference: xxx
#'
#'#' Reference: Edelman JH. (1972). Groundwater hydraulics of extensive aquifers.
#' Bulletin 13. Wageningen, Netherlands.: International Institute for Land
#' Reclamation and Improvement (ILRZ).
#'
#' \deqn{s + t}
#'
#' @param t time (T)
#' @param Kh horizontal transmissivity (L/T)
#' @param D saturated thickness (level in rest) (L)
#' @param S storage (-)
#' @param s drawdown at the border of the cross section (L)
#'
#' @export
#'
#' @return Q abstraction (L^3/T)
#-------------------------------------------------------------------------------
edelman_Q <- function(t, Kh, D, S, s) {


  Q <- s * sqrt((S * Kh * D) / (pi * t))

  return(Q)
}
