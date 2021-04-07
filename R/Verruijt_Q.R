#' Verruijt - flow
#'
#' @description Verruijt flow in a freatic aquifer with stationary flow.
#' Reference: xxx
#'
#' @param r_eq equivalent radius construction pit (L)
#' @param r0 radius of influence (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param H saturated thickness (level in rest) (L)
#' @param s drawdown at the border of the construction pit (L)
#' @param Rech recharge (L/T)
#'
#' @export
#'
#' @return Q abstraction (L^3/T)
#-------------------------------------------------------------------------------
Verruijt_Q <- function(r_eq, r0, Kh, H, s, Rech) {

  Q <- - (H^2 - (H - s)^2 + (Rech / (2 * Kh)) * (r0^2 - r_eq^2)) * (pi * Kh) /
    log(r_eq / r0)

  return(Q)
}
