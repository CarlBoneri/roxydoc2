#' Generate preformatted param roxygen lines from a function name.
#'
#'
#' \code{docr.param}
#'
#'
#'
#' @family Documentation functions
#' @export
docr.param <- function(fun_name = NULL){

  all_argz <- formalArgs(fun_name)

  prep_d <- sprintf("@param %s", all_argz)

  prep_d
}
