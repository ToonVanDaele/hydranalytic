#' Thiem / Dupuit - flow
#'
#' @description Thiem Dupuit flow in an unconfined aquifer with stationary flow
#' (Dupuit, 1857).
#'
#' Reference: Dupuit J (1857) Mouvement de l’eau a travers le terrains permeables.
#' C R Hebd Seances Acad Sci 45: 92–96.
#'
#' @param r_eq equivalent radius construction pit (L)
#' @param r0 radius of influence (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param H saturated thickness (level in rest) (L)
#' @param s drawdown at the border of the construction pit (L)
#'
#' @export
#'
#' @return Q abstraction (L^3/T)
#-------------------------------------------------------------------------------
dupuit_Q <- function(r_eq, r0, Kh, H, s) {


  Q <- (pi * Kh * (H^2 - (H - s)^2)) / (log(r0 + r_eq) - log(r_eq))

  return(Q)
}
