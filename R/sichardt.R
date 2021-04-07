#' Sichardt
#'
#' @description Sichardt (Vandebroek et al., 2011). This formula is an emperic
#' formula. It drawdown and conductivity should be expressed in meter (m) and
#' meter per second (m/s) respectivily.
#'
#' @param s drawdown (m)
#' @param K hydraulic conductivity (m/s)
#'
#' @export
#'
#' @return R sichardt radius of influence (m)

sichardt <- function(s, k) {
  R <- 3000 * s * k^ (1 / 2)
  return(R)
}
