#' Besselfunction
#'
#' @description Besselfunction
#'
#' @param r distance to well (L)
#' @param L leakage(L?)
#'
#' @return bess bessel (-?)
#'
#' @examples
#' Bessel(20, 5)
#'
#' @references
#'
#' @export
#'
Bessel <- function (Q, D, Kh, r, Dc, Kv) {

  L <- sqrt(Kh * D * Dc / Kv)

  s <- (Q / 2 * pi * Kh * D ) * Bess2_0(r / L)


  return(s)
}
