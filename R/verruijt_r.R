#' Verruijt - radius to a given drawdown
#'
#' @description Verruijt unconfined aquifer stationary flow with recharge.
#'
#'
#' @param r0 radius of influence (L)
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param Rech recharge (L/T)
#' @param s drawdown (L)
#'
#' @importFrom stats uniroot
#'
#' @export
#'
#' @return r distance (L)

#------------------------------------------------------------------------------
verruijt_r  <- function(r0, Q, D, Kh, Rech, s){

  r <- uniroot(function(r, s0 = s) verruijt_s(r = r, r0 = r0, Q = Q, D = 10, Kh = 5,
                                                  Rech = Rech) - s0,
          interval = c(1e-300, 1e30))
  return(r[[1]])
}
