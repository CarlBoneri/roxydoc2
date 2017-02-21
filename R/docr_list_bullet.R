docr.list_bullet <- function(L){
  slots <- unlist(lapply(1:length(L),function(i){
    sprintf("#'   \\item %s", L[[i]])
  }))

  sprintf("#' \\itemize{\n%s\n#' }", paste0(slots,collapse="\n"))
}
