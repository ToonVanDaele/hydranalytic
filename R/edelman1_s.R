#' Edelman - case 1
#'
#' @description
#'
#' Reference: Edelman JH. (1972). Groundwater hydraulics of extensive aquifers.
#' Bulletin 13. Wageningen, Netherlands.: International Institute for Land
#' Reclamation and Improvement (ILRZ).
#'
#' @param r distance (L)
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
#-------------------------------------------------------------------------------
edelman_s <- function(r, S, Kh, D, t, h0) {

  s <- h0 * (1 - erf(r * (sqrt(S / (4 * Kh * D * t)))))

  return(s)

}
