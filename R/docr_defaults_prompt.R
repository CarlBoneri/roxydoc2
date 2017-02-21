#' Console prompting for docr default option settings
#'
#' \code{docr.defaults_prompt}
#'
#'
#'
#' @family Documentation functions
#'
#' @export
docr.defaults_prompt <- function(...){
  if(!docr.has_defaults()){
    set_names <- names(formals(docr.defaults))
    dfl_args <- sapply(set_names, function(i){
      to_prompt <- sprintf("%s: ", i)
      init <- readline(to_prompt)
      if(nchar(init) == 0){
        init <- NA
      }
      return(init)
    })

    do.call('docr.defaults', as.list(dfl_args))
  }
}
