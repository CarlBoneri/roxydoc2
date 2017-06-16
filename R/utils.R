#' Create a string of #
#' 
#' \code{pound}
#' 
#' 
#' @examples 
#' 
#' > pound(n = 14)
#' [1] "##############"
#' > pound(n = 14, "--")
#' [1] "----------------------------"
pound <- function(n = NULL, use_symbol = "#"){
  parts <- rep(use_symbol, n)
  stringi::stri_join(parts, collapse = "")
}
