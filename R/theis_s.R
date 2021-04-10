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
#' @param W_u_method approximation method for Well function
#'
#' @return s drawdown (L)
#'
#' @examples
#' theis_s(Q = 500, t = 3, r = 10, Kh = 2, D = 10, S = 0.001)
#'
#' @export
#'
theis_s <- function(Q, t, r, Kh, D, S, W_u_method = "srivastava") {

  u <- (S * r^2) / (4 * Kh * D * t)

  W_u <- W_u(u, W_u_method)

  s <- Q / (4 * pi * Kh * D) * W_u

  s <- ifelse(t == 0, 0, s)

  ifelse(r == 0 & s == Inf,
         warning("at distance (r) = 0 drawdown (s) becomes Inf"), "")

  return(s)
}
