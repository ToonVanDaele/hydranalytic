#' Verruijt - drawdown
#'
#' @description drawdown in an unconfed aquifer with stationary flow and
#' recharge (Verruijt, 1970). page 55 equation 5.20
#'     When r > r0 the drawdown (s) is set to 0.
#'     When the drawdown (s) is larger than the aquifer thickness (D) the
#'     drawdown is set equal to the the thickness and a warning message is
#'     returned.
#'
#' Verruijt A. (1970). Theory of Groundwater Flow. London: Macmillan.
#'
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param Rech recharge (L/T)
#' @param r distance at which to calculate the drawdown(L)
#' @param r0 radius of influence(L)
#'
#' @export
#'
#' @return s drawdown (L)

#------------------------------------------------------------------------------
verruijt_s <- function(r, r0, Q, D, Kh, Rech) {

  s <- ifelse(r >= r0,
               0,
               ifelse(D^2 - ((Q / (2 * pi * Kh)) * (log(Q / (pi * Rech * r^2)) - 1)) - ((Rech * r^2) / (2 * Kh)) > 0,
                       D - (D^2 - ((Q / (2 * pi * Kh)) * (log(Q / (pi * Rech * r^2)) - 1)) - ((Rech * r^2) / (2 * Kh)))^0.5,
                       D))

  return(s)
}
