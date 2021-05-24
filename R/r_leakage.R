#' Leakage equation
#'
#' Reference: Handbook: Ground Water and Wellhead Protection. EPAl625/R-94/001
#' September 199. US Environmental Protection Agency. equation 4.18 p. 81
#'
#' @param Q abstraction (L^3/T)
#' @param Kh hydraulic conductivity (L/T)
#'
#' @return radius of influence (L)
#' @export
#'

r_leakage_steady <- function(Q, K) {

  r <- ((Q / K) * pi )^0.5
  return(r)
}
