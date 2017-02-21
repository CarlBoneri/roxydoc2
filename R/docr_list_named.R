#' @export
docr.list_named <- function(L){

  list_body <- lapply(names(L), function(i){

    var_body <- stri_wrap(L[[i]], width = 60, indent = 0, exdent = 6)

    var_title <- sprintf("#'   \\item{%s}", i)

    paste0(var_title,
           sprintf("{%s}",
                   paste0(var_body,
                          collapse = "\n#'")
           )
    )

  }) %>% paste0(collapse = "\n")

  sprintf("#' \\describe{\n%s\n#' }",list_body) %>% HTML()
}
