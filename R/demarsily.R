#' @description  De Marsily, 1986; p.198
#' (www.grondwaterformules.nl)
#'
#' @param x x
#' @param Sy Sy (-)
#' @param kD kD (L/T)
#' @param t time (T)
#' @param h0 h0 (L)
#'
#' @export
#'
#' @return demarsily demarsily (?)
demarsily <- function(x, Sy, kD, t, h0) {

  h0 * erf( x * (sqrt( Sy / (4 * kD * t))))

}
