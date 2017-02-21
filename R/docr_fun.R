#' Formetting for functions in alternate packages
#'
#'
#' \code{docr.fun}
#'
#'
#' @examples
#' > docr.fun(grep)
#' [1] "\\code{\\link[base]{grep}}"
#' > docr.fun(rt.css_prep_idx)
#' [1] "\\code{\\link[rtspecs]{rt.css_prep_idx}}"
#' > docr.fun(select)
#' [1] "\\code{\\link[dplyr]{select}}"
#'
#'
#' @family Documentation functions
#'
#' @author Carl S.A. Boneri, \email{carl.boneri@@whyles.com}
#'
#'
#' @export
docr.fun <- function(...){
  ds.name <- function(x){
    ifelse(!is.character(x),deparse(substitute(x)), x)
  }

  set_fun <- ds.name(...)

  fun_home <- gsub("package:", "", find(set_fun))

  if(fun_home == ".GlobalEnv"){
    fun_home <- basename(getwd())
  }
  sprintf('\\code{\\link[%s]{%s}}', fun_home, set_fun)
}
