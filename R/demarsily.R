#' DeMarsily
#'
#' @description  De Marsily, 1986; p.198
#' (www.grondwaterformules.nl)
#'
#' @param x distance (L)
#' @param S Storage (-)
#' @param Kh Kh (L/T)
#' @param D thickness (L)
#' @param t time (T)
#' @param h0 change of water level in  (L)
#'
#'
#' @return s change of groundwater level (L)
#'
#' @export
demarsily <- function(x, S, Kh, D, t, h0) {

  s <- h0 * (1 - erf( x * (sqrt( S / (4 * Kh * D * t)))))

  return(s)

}
