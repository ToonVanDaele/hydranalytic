#' @description Calculation of u for W(u) in the Theis formula
#'
#' @param t time (T)
#' @param r radius (L),
#' @param kD conductivity (L^2/T)
#' @param S storage (-)
#'
#' @export
#'
#' @return u (-)
#----------------------------------------------------------------------
u_theis <- function (t, r, kD, S) {

  u_theis <- (S * r^2) / (4 * kD * t)
  return (u_theis  )
}
