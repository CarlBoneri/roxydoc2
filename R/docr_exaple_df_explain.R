docr.exaple_df_explain <- function(Fun, comments = NULL){
  a <- sprintf("#' %s",Fun)
  b <- eval(parse(text = substitute(Fun)))
  HTML(
    c(docr.describe_df(b),
      "\n#'\n",
      docr.example_df(b, comments = comments,
                      write_fun = Fun)
    )
  )
}
