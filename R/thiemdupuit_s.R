#' Thiem dupuit
#'
#' @description Thiem Dupuit drawdown in a freatic aquifer with stationary flow. Reference: xxx
#'
#' @param r distance (L)
#' @param r0 radius of influence (L)
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal transmissivity (L/T)
#'
#' @export
#'
#' @return s drawdown (L)
#-----------------------------------------------------------------------------------
ThiemDupuit <- function(r, r0, Q, D, Kh) {

  s <- ifelse (r < r0, (Q / (2 * pi * Kh * D)) * log ( r0 / r), 0)
  return (s)
}
