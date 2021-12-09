#' Theis - distance to drawdown
#'
#' @description non-stationary drawdown at a distance (r) with the Theis equation (1935) in an
#' unconfined aquifer.
#'
#' By default the Srivastava approximation is used for the well function W(u).
#' The Huisman approximation is not appropiate for small values for u (i.e
#' small drawdown and far from the well).
#'
#' Reference: Theis, C.V. (1935) The relation between the lowering of the
#' piezometric surface and the rate and duration of discharge of a well using
#' groundwater storage. Transactions American Geophysical Union 16: 519-524.
#'
#' @param Q abstraction (L^3/T)
#' @param t time (T),
#' @param s drawdown (L)
#' @param Kh horizontal hydraulic conductivity (L/T)
#' @param D aquifer thickness
#' @param S storage (-)
#' @param W_u_method approximation method for Well function
#'
#' @return r distance (L)
#'
#' @examples
#' theis_r(Q = 500, t = 3, s = 0.05, Kh = 2, D = 10, S = 0.001)
#'
#' @export
#'

theis_r <- function(Q, t, s, Kh, D, S, W_u_method = "srivastava"){
  r <- uniroot(function(r, s) theis_s(Q = Q, r = r, t = t, Kh = Kh, D = D, S = S,
                                 W_u_method = W_u_method) - s,
          interval = c(1e-30, 1e30), s = s)
  return(r[[1]])
}
