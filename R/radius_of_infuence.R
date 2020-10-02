#' Radius of influence
#'
#' @description Radius of influence for a stationary groundwater well in a freatic aquifer with net recharge. Reference: De Smedt, 2007
#'
#' @param Q abstraction (L^3/T)
#' @param Rech recharge (L/T)
#'
#' @return radius of influence (L)
#' @export
#'

r_freat_steady <- function(Q, Rech) {

  r <- (Q / (pi*Rech))^0.5
  return(r)
}


#' @description Area of influence for a stationary groundwter well in a freatic aquifer with net recharge. Reference: De Smedt, 2007
#'
#'@param Q abstraction (L^3/T)
#'@param Rech recharge (L^3/T)
#'
#'@return area of influence (L^2)

a_freat_steady <- function(Q, Rech) {

  A <- Q / Rech
  return(A)
}

