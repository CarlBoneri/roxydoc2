#' Set a formatted family name
#'
#'
#' \code{docr.family}
#'
#' @param family_name string The name of the family the function belongs to.
#'
#'
#' @return A formatted roxygen string
#'
#' @family Documentation functions
#'
#' @author Carl S.A. Boneri, \email{carl.boneri@@whyles.com}
#'
#' @examples
#'
docr.family <- function(family_name = NULL){
  docr.wrap(
    paste0(c("@family",family_name, "functions"),
           sep = " ", collapse = "")
  )
}
