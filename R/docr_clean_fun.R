#' Build a clean and set-formatted function
#'
#'
#' \code{\link{docr.clean_fun}}
#'
#'
#'
#' If a function is written at the command line, or in a file with no
#' documentation, this function will take the function name, in a quoted
#' variable state, and rebuild the function in the same-formatted structure
#' as when it was initially written.
#'
#' @param fun_name The name of the function to be rebuilt
#'
#'
#' @family Documentation functions
#'
#' @author Carl S.A. Boneri, \email{carl@@gmail.com}
#'
#' @return The structured function.
#'
#'
#' @examples
#'
#' # From the Base package \code{\link{grep}}
#'
#' > docr.clean_fun('grep')
#' grep <- function (pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
#'                   fixed = FALSE, useBytes = FALSE, invert = FALSE)
#' {
#'   if (!is.character(x))
#'     x <- structure(as.character(x), names = names(x))
#'   .Internal(grep(as.character(pattern), x, ignore.case, value,
#'                  perl, fixed, useBytes, invert))
#' }
#'
#' # From the command line..we create the following:
#' paste.html <- function(x){
#'    htmltools::HTML(paste0(x, collapse = "\n"))
#' }
#'
#' # Notice the output is the same as the written style, to match the author's
#' # preference.
#'
#' > docr.clean_fun('paste.html')
#' paste.html <- function(x){
#'  htmltools::HTML(paste0(x, collapse = "\n"))
#' }
#'
#' @export
#' @importFrom htmltools HTML
docr.clean_fun <- function(fun_name = NULL){
  tmp_fun_name <- fun_name
  FUN <- HTML(
    paste0(tmp_fun_name," <- ",
           HTML(
             paste0(deparse(get(fun_name),
                            control="all"),
                    collapse="\n"))
    ))
  return(FUN)
}
