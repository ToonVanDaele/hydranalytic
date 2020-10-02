#' @description Calculation of W(u) with the simple approximation (Huisman).
#'    reference www.grondwaterformules.nl - Huisman, 1972 p.115
#'    Don't use this approximation. Only usefull for comparision with older calculation
#' @param u u (-)
#' @export
#'
#' @return W_u W_u (-)

W_u_Huisman <- function (u) {

  W_u_Huisman <- log(0.562 / u)

  return (W_u_Huisman)
}
