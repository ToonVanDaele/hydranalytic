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
theis_s <- function (Q, t, r, Kh, D, S, W_u_method = "srivastava") {

  require(assertthat)

  assert_that(all(t >= 0), all(r >= 0), all(Kh > 0), all(D > 0), all(S >= 0))
  maxl <- max(length(Q), length(t), length(r), length(Kh), length(D),
              length(S), length(W_u_method))

  assert_that(length(Q) == maxl | length(Q == 1))
  assert_that(length(t) == maxl | length(t == 1))
  assert_that(length(r) == maxl | length(r == 1))
  assert_that(length(Kh) == maxl | length(Kh == 1))
  assert_that(length(D) == maxl | length(D == 1))
  assert_that(length(S) == maxl | length(S == 1))
  assert_that(length(W_u_method) == maxl | length(W_u_method == 1))

  Q <- ifelse(length(Q) == 1, rep(Q, maxl), Q)
  t <- ifelse(length(t) == 1, rep(t, maxl), t)
  r <- ifelse(length(r) == 1, rep(r, maxl), r)
  Kh <- ifelse(length(Kh) == 1, rep(Kh, maxl), Kh)
  D <- ifelse(length(D) == 1, rep(D, maxl), D)
  S <- ifelse(length(S) == 1, rep(S, maxl), S)
  W_u_method <- ifelse(length(W_u_method) == 1, rep(W_u_method, maxl), W_u_method)

  u <- (S * r^2) / (4 * Kh * D * t)

  W_u <- W_u(u, W_u_method)

  s <- Q / (4 * pi * Kh * D) * W_u

  s <- ifelse(t == 0, 0, s)

  if(r == 0 & s == Inf) warning('at distance (r) = 0 drawdown (s) becomes Inf')

  return (s)
}
