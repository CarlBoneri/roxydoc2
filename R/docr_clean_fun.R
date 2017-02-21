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
#' @param trim_blanks Should any carriage returns or empty lines be removed from
#'  the source code.
#'
#' @param rm_comments Should the comments in the source script of the function
#'  be removed.
#'
#' @param do_cat Should the output be printed to the console by calling
#'  \code{\link{cat}}.
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
docr.clean_fun <- function(fun_name = NULL, trim_blanks = FALSE,
                           rm_comments = FALSE, do_cat = TRUE){
  deparse(get(fun_name), control = "all") %>% (function(x){
    x[[1]] <- sprintf("%s <- %s",fun_name, x[[1]])

    if(trim_blanks){
      x <- x[nchar(x) > 0]
    }

    if(rm_comments){
      x <- grep("^[[:space:]]{1,}\\#", x, invert = TRUE, value = TRUE)
    }
    if(do_cat){
      cat(x, sep = "\n")
    }else {
      x
    }
  })
}
