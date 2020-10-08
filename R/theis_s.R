#' Theis_s
#'
#' @description Calculation of drawdown (s) with the Theis equation.
#'     By default the Srivastava equation is used for the W(u) approximation.
#'
#' @param Q abstraction (L^3/T)
#' @param t time (T),
#' @param r radius (L)
#' @param Kh horizontal hydraulic conductivity (L/T)
#' @param D aquifer thickness
#' @param S storage (-)
#'
#'
#' @return s drawdown (L)
#'
#' @examples
#' theis_s(Q = 500, t = 3, r = 10, Kh = 2, D = 10, S = 0.001)
#'
#' @export
#'
theis_s <- function (t, Q, r, Kh, D, S, W_u_method = "srivastava") {

  Kh2 <- Kh
  D2 <- D

  require(assertthat)


  assert_that(t >= 0, r >= 0, Kh > 0, D > 0, S >= 0)

  u <- (S * r^2) / (4 * Kh * D * t)

  W_u <- ifelse(W_u_method == "huisman", W_u_Huisman(u), W_u_srivastava(u))

  s <- Q / (4 * pi * Kh2 * D2) * W_u

  return (s)
}
