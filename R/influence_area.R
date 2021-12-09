#' Equal recharge area
#'
#' @description The area for a stationary groundwater well were extraction
#' equals net recharge. Reference: De Smedt, 2007
#'
#' @param Q abstraction (L^3/T)
#' @param Rech recharge (L^3/T)
#'
#' @return area (L^2)
#'
#' @examples
#' freat_steady_a(50, 0.2)
#'
#' @export

freat_equal_a <- function(Q, Rech) {

  A <- Q / Rech
  return(A)
}
