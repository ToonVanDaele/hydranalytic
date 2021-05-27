#' Thiem / Dupuit - drawdown
#'
#' @description Thiem / Dupuit drawdown in an unconfined aquifer with stationary
#' flow.
#'
#' Reference: Dupuit J (1857) Mouvement de l’eau a travers le terrains permeables.
#' C R Hebd Seances Acad Sci 45: 92–96.
#'
#' @param r distance (L)
#' @param r0 radius of influence (L)
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal conductivity (L/T)
#'
#' @export
#'
#' @return s drawdown (L)
#-------------------------------------------------------------------------------
dupuit_s2 <- function(r, r0, Q, D, Kh) {

  s <- ifelse(r < r0,
              D - (D^2 - (Q / (pi * Kh)) * log(r0 / r))^0.5,
              0)
  return(s)
}
