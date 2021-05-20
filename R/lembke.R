#' Lembke radius of influence
#'
#' @description Lembke (1887). Empirical formula for radius of influence
#' meter per second (m/s) respectively.
#'
#' @param D saturated thickness (L)
#' @param k hydraulic conductivity (L/T)
#' @param Rech Recharge (L/T)
#'
#' @export
#'
#' @return r Lembke radius of influence (m)

lembke <- function(s, k) {
  r <- D * sqrt(k / 2 * Rech)
  return(r)
}
