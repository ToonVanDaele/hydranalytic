#' Thiem dupuit verruijt search for radius with drawdown = s
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
#' @param s drawdown (L)
#'
#' @export
#'
#' @return r distance (L)

#-----------------------------------------------------------------------------------
radius_ThiemDupuitVerruijt <- function (r0, Q, D, Kh, Rech, s) {

  r <- ifelse(ThiemDupuitVerruijt_0(r = 0.001, r0 = r0, Q = Q, D = D, Kh = Kh, Rech = Rech, s = s) < 0,
              uniroot(f = ThiemDupuitVerruijt_0 , r0 = r0, Q = Q, D = D, Kh = Kh, Rech = Rech, s = s,
                      lower = 0.001, upper = r0),
              0.001)
  return (r[[1]])
}
