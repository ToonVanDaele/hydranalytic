#' Theis_r0_aprx
#'
#' @description Approximated radius of influence with the Theis equation.
#'
#' @param t time (T)
#' @param Kh horizontal hydraulic conductivity (L/T)
#' @param D thickness of saturated (L)
#' @param S storage (-)
#'
#' @return r radius (L)
#'
#' @export
#'
theis_r0_aprx <- function (t, Kh, D, S) {

  r <- sqrt(2.25 * Kh * D * t / S)

  return (r)
}
