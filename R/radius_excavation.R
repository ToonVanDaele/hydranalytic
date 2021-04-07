#' equivalente radius of a rectangular excavation
#'
#' @description none yet

#'
#' @param L length (L)
#' @param W width (L)
#'
#' @return r radius (L)

eq_radius <- function(L, W) {
  r <- (L + W) / pi
  return(r)
}
