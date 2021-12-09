#' Dupuit - radius to a given drawdown
#'
#' @description Dupuit unconfined aquifer stationary flow with recharge.
#'
#'
#' @param r0 radius of influence (L)
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param s drawdown (L)
#'
#' @importFrom stats uniroot
#'
#' @export
#'
#' @return r distance (L)

#------------------------------------------------------------------------------
dupuit_r  <- function(r0, Q, D, Kh, s){

  r <- uniroot(function(r, s0 = s) dupuit_s(r = r, r0 = r0, Q = Q, D = D, Kh = Kh) - s0,
          interval = c(1, r0))
  return(r[[1]])
}
