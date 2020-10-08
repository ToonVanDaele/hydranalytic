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

  # assert_that(t >= 0)
  # assert_that(r >= 0)
  # assert_that(Kh > 0)
  # assert_that(D > 0)
  # assert_that(S >= 0)

  u <- u_theis(t = t, r = r, Kh = Kh, D = D, S = S)

  W_u <- ifelse(W_u_method == "huisman", W_u_Huisman(u), W_u_srivastava(u))

  s <- Q / (4 * pi * Kh2 * D2) * W_u

  return (s)
}
