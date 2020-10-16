#' DeMarsily
#'
#' @description  De Marsily, 1986; p.198
#' (www.grondwaterformules.nl)
#'
#' @param x distance (L)
#' @param Sy Sy (-)
#' @param Kh Kh (L/T)
#' @param D thickness (L)
#' @param t time (T)
#' @param h0 change of water level in  (L)
#'
#'
#' @return s change of groundwater level (L)
#'
#' @export
demarsily <- function(x, Sy, kD, t, h0) {

  s <- h0 * erf( x * (sqrt( Sy / (4 * kD * t))))

  return(s)

}
