#' Edelman effect drawdown
#'
#' @description Edelman horizontal flow towards a canal / building site. Reference: xxx
#'
#' @param r distance (L)
#' @param D saturated thickness (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param t time (T)
#' @param S storage (-)
#' @param h0 change of water level in  (L)
#'
#' @export
#'
#' @return s drawdown (L)
#-----------------------------------------------------------------------------------
edelman <- function(r, D, Kh, t, S) {

  s <- h0 * erf(r * sqrt( S / 4 * Kh * D * t))

  return (s)
}



edelman_rise <- function(r, D, Kh, t, S) {

  s <- h0 * erfc(r * sqrt( S / 4 * Kh * D * t))

  return (s)
}
