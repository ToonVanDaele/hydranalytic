#' Verruijt - flow
#'
#' @description Verruijt flow in an unconfined aquifer with stationary flow.
#' (Verruijt, 1970). page 55 equation 5.19.
#'
#' Verruijt A. (1970). Theory of Groundwater Flow. London: Macmillan.
#'
#' @param r_eq radius of well (L) or equivalent radius of site (L)
#' @param r0 radius of influence (L)
#' @param Kh horizontal conductivity (L/T)
#' @param H saturated thickness (level in rest) (L)
#' @param s drawdown at the border of the construction pit (L)
#' @param Rech recharge (L/T)
#'
#' @export
#'
#' @return Q abstraction (L^3/T)
#-------------------------------------------------------------------------------
verruijt_Q <- function(r_eq, r0, Kh, H, s, Rech) {

  Q <- - (H^2 - (H - s)^2 + (Rech / (2 * Kh)) * (r0^2 - r_eq^2)) * (pi * Kh) /
    log(r_eq / r0)

  return(Q)
}
