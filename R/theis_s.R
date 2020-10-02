#' @description Calculation of drawdown (s) with the Theis equation.
#'     By default the Srivastava equation is used for the W(u) approximation.
#'
#' @param Q abstraction (L^3/T)
#' @param t time (T),
#' @param r radius (L)
#' @param kD conductivity (L^2/T)
#' @param S storage (-)
#'
#' @export
#'
#' @return s drawdown (L)
#'

s_theis <- function (Q, t, r, kD, S, W_u_method = "Srivastava") {

  u <- u_theis(t = t, r = r, kD = kD, S = S)

  W_u <- ifelse (W_u_method == "Huisman",
                 W_u_Huisman(u),
                 W_u_srivastava(u))

  s <- Q / (4*pi*kD) * W_u

  return (s)
}
