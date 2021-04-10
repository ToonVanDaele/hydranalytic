#' @title Calculation of W(u)
#'
#' @description W(u) is calculated using the Srivastava approximation (default)
#' or Huisman approximation.
#' @param u (-)
#' @param method approximation method for W(u)
#'
#' @return W_u (-)
#' @export

W_u <- function(u, method = "srivastava") {

  W_u <- ifelse(method == "srivastava" | is.null(method),
            ifelse(u < 1, log(exp(-0.5772) / u) + (0.9653 * u) - (0.169 * u^2),
                   1 / (u * exp(u)) * (u + 0.3575) / (u + 1.28)),
            log(0.562 / u))

  return(W_u)
}
