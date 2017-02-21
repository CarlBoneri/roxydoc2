#' Check if the default options have been set
#'
#' \code{docr.has_defaults}
#'
#'
#' @return logical
#' \describe{
#'   \item{TRUE}{if the default options exists/ have been set}
#'   \item{FALSE}{The user default options have not been set}
#' }
#'
#' @family Documentation functions
#'
#' @export
docr.has_defaults <- function(){
  !is.null(getOption('docr'))
}
