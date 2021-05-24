#' Sichardt radius of influence
#'
#' @description Sichardt (Vandebroek et al., 2011). This formula is an empirical
#' formula. Drawdown and conductivity should be expressed in meter (m) and
#' meter per second (m/s) respectively.
#'
#' @param s drawdown (m)
#' @param k hydraulic conductivity (m/s)
#'
#' @export
#'
#' @return R Sichardt radius of influence (m)

sichardt <- function(s, k) {
  R <- 3000 * s * k^ (1 / 2)
  return(R)
}
