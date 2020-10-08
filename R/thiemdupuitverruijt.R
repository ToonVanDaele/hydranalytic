#' Thiem dupuit verruijt
#'
#' @description Thiem / dupuit / Verruijt freatic aquifer stationary flow with recharge.
#'     When r > r0 the drawdown (s) will be = 0
#'     When the drawdown (s) is larger than the aquifer thickness (D) the drawdown is set equal to the the thickness.
#'     A warning message is returned.
#'
#' @param Q abstraction (L^3/T)
#' @param D saturated thickness (L)
#' @param Kh horizontal transmissivity (L/T)
#' @param Rech recharge (L/T)
#' @param r distance (L)
#'
#' @export
#'
#' @return s drawdown (L)

#-----------------------------------------------------------------------------------
ThiemDupuitVerruijt <- function (r, r0, Q, D, Kh, Rech) {

  s <- ifelse (r >= r0,
               0,
               ifelse (D^2 - ((Q/(2*pi*Kh)) * (log(Q/(pi*Rech*r^2)) -1)) - ((Rech*r^2) / (2*Kh))  > 0,
                       D - (D^2 - ((Q/(2*pi*Kh)) * (log(Q/(pi*Rech*r^2)) -1)) - ((Rech*r^2) / (2*Kh)))^0.5,
                       D))
  return(s)
}