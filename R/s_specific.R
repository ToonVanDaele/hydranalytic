#' specific storages
#'
#' @description specific elastic storage. Storage only due to compressability.
#'    Formule van der Gun (Van der Gun, 1979)
#'  reference: Lebbe & Vandebohede (2004)
#'
#' @param d depth below ?maaiveld? (L)
#'
#' @export
#'
#' @return

Sspec <- function(d) {

  Sspec <- 1.8E-6 + 2.59E-4 * d^-0.7
  return(Sspec)
}


