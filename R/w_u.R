#' Calculation of u for Theis formula
#'
#' @description Calculation of u for W(u) in the Theis formula
#'
#' @param t time (T)
#' @param r radius (L),
#' @param Kh horizontal hydraulic conductivity (L/T)
#' @param D aquifer thickness (L/T)
#' @param S storage (-)
#'
#' @export
#'
#' @return u (-)
#----------------------------------------------------------------------
u_theis <- function (t, r, Kh, D, S) {

  u <- (S * r^2) / (4 * Kh * D * t)
  return (u)
}

