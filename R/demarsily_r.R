#' DeMarsily  -  distance to drawdown
#'
#' @description  De Marsily, 1986; p.198
#' (www.grondwaterformules.nl)
#'
#' @param s drawdown (L)
#' @param S Storage (-)
#' @param Kh Kh (L/T)
#' @param D thickness (L)
#' @param t time (T)
#' @param h0 change of water level at cross section (L)
#'
#'
#' @return r change of groundwater level (L)
#'
#' @export
demarsily_r <- function(s, S, Kh, D, t, h0) {

  r <- erfcinv(s / h0) / sqrt(S / (4 * Kh * D * t))

  return(r)

}
