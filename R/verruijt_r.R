#' Verruijt search for radius with drawdown = s
#'
#' @description Thiem / dupuit / Verruijt freatic aquifer stationary flow
#' with recharge.
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
radius_ThiemDupuitVerruijt <- function(r0, Q, D, Kh, Rech, s) {

  r <- ifelse(ThiemDupuitVerruijt_0(r = 0.001, r0 = r0, Q = Q, D = D, Kh = Kh,
                                    Rech = Rech, s = s) < 0,
              uniroot(f = ThiemDupuitVerruijt_0, r0 = r0, Q = Q, D = D, Kh = Kh,
                      Rech = Rech, s = s, lower = 0.001, upper = r0),
              0.001)
  return(r[[1]])
}


ThiemDupuitVerruijt_r <- function(r0, Q, D, Kh, Rech, s){

  r <- uniroot(function(r, s0 = s) ThiemDupuitVerruijt(r = r, r0 = r0, Q = Q, D = 10, Kh = 5,
                                                  Rech = Rech) - s0,
          interval = c(1e-300, 1e30))
  return(r[[1]])
}
