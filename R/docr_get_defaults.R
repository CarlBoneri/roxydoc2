#' Extract the global options associated with docr user
#'
#' \code{docr.get_defaults}
#'
#'
#'
#'
#' @return
#'
#' \format{
#'  A data frame with 1 observations on the following 5 variables.
#'  \describe{
#'    \item{\strong{author}}{\emph{character} formatted vector with \emph{1}
#'    unique values.}
#'    \item{\strong{email}}{\emph{character} formatted vector with \emph{1}
#'    unique values.}
#'    \item{\strong{company}}{\emph{character} formatted vector with \emph{1}
#'     unique values.}
#'    \item{\strong{website}}{\emph{character} formatted vector with \emph{1}
#'    unique values.}
#'    \item{\strong{github}}{\emph{character} formatted vector with \emph{1}
#'     unique values.}
#'   }
#' }
#'
#'
#'  @family Documentation functions
#'
#'
#' @export
docr.get_defaults <- function(field = NULL){
  ref <- as.data.frame(getOption('docr'), stringsAsFactors = FALSE)

  if(!is.null(field)){
    ref[, agrep(field, colnames(ref), ignore.case = TRUE)]
  }else{
    ref
  }
}
