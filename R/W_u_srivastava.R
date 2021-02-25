#' W_u_Srivastava
#'
#' @description Calculation of W(u) Srivastava approximation
#' @param u (-)
#'
#' @export
#'
#' @return W_u (-)
#----------------------------------------------------------------------
W_u_srivastava <- function(u) {

  W_u_srivastava <- ifelse(u < 1, log(exp(-0.5772) / u) + (0.9653 * u) -
                             (0.169 * u^2),
                           1 / (u * exp(u)) * (u + 0.3575) / (u + 1.28))

  return(W_u_srivastava)
}
