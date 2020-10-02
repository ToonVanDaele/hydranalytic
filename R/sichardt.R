#' @description Sichardt (Vandebroek et al., 2011). This formula is an emperic formula.
#' It drawdown and conductivity should be expressed in meter (m) en meter per second (m/s) respectivily
#'
#' @param s drawdown (m)
#' @param kD conductivity doorlatendheid (m/s)
#'
#' @export
#'
#' @return sichardt sichardt (?)

sichardt <- function(s, k) {
  R <- 3000 * s * k^(1/2)
  return(R)
}
